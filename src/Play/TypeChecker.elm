module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata exposing (Metadata)
import Play.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Data.TypeSignature as TypeSignature exposing (TypeSignature)
import Play.Qualifier as Qualifier
import Play.TypeChecker.Problem exposing (Problem(..))
import Set exposing (Set)


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type TypeDefinition
    = CustomTypeDef String SourceLocationRange (List String) (List ( String, Type ))
    | UnionTypeDef String SourceLocationRange (List String) (List Type)


type alias WordDefinition =
    { name : String
    , type_ : WordType
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatch SourceLocationRange Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type AstNode
    = IntLiteral SourceLocationRange Int
    | Word SourceLocationRange String WordType
    | WordRef SourceLocationRange String
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type
    | Builtin SourceLocationRange Builtin


type alias Context =
    { types : Dict String TypeDefinition
    , typedWords : Dict String WordDefinition
    , untypedWords : Dict String Qualifier.WordDefinition
    , stackEffects : List StackEffect
    , boundGenerics : Dict String Type
    , boundStackRanges : Dict String (List Type)
    , callStack : Set String
    , errors : List Problem
    }


type StackEffect
    = Push Type
    | Pop Type


initContext : Qualifier.AST -> Context
initContext ast =
    let
        concreteTypes =
            Dict.map
                (\_ t ->
                    case t of
                        Qualifier.CustomTypeDef name range generics members ->
                            CustomTypeDef name range generics members

                        Qualifier.UnionTypeDef name range generics memberTypes ->
                            UnionTypeDef name range generics memberTypes
                )
                ast.types

        genericErrors t =
            let
                collectReferencedGenerics memberTypes =
                    List.map Type.referencedGenerics memberTypes
                        |> List.foldl Set.union Set.empty
                        |> Set.toList

                collectUndeclaredGenericProblems range listedGenerics memberTypes =
                    memberTypes
                        |> List.filter (\gen -> not (Set.member gen listedGenerics))
                        |> List.map (\gen -> UndeclaredGeneric range gen listedGenerics)
            in
            case t of
                CustomTypeDef name range generics members ->
                    let
                        listedGenerics_ =
                            Set.fromList generics
                    in
                    members
                        |> List.map Tuple.second
                        |> collectReferencedGenerics
                        |> collectUndeclaredGenericProblems range listedGenerics_

                UnionTypeDef name range generics mts ->
                    let
                        listedGenerics_ =
                            Set.fromList generics
                    in
                    mts
                        |> collectReferencedGenerics
                        |> collectUndeclaredGenericProblems range listedGenerics_
    in
    { types = concreteTypes
    , typedWords = Dict.empty
    , untypedWords = ast.words
    , stackEffects = []
    , boundGenerics = Dict.empty
    , boundStackRanges = Dict.empty
    , callStack = Set.empty
    , errors = List.concatMap genericErrors (Dict.values concreteTypes)
    }


run : Qualifier.AST -> Result (List Problem) AST
run ast =
    typeCheckHelper (initContext ast) ast


typeCheckHelper : Context -> Qualifier.AST -> Result (List Problem) AST
typeCheckHelper context ast =
    let
        updatedContext =
            Dict.foldl (\_ v acc -> typeCheckDefinition v acc) context ast.words
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = updatedContext.types
            , words = updatedContext.typedWords
            }

    else
        Err updatedContext.errors


typeCheckDefinition : Qualifier.WordDefinition -> Context -> Context
typeCheckDefinition untypedDef context =
    case Dict.get untypedDef.name context.typedWords of
        Just _ ->
            cleanContext context

        Nothing ->
            case untypedDef.implementation of
                Qualifier.SoloImpl impl ->
                    typeCheckSoloImplementation context untypedDef impl

                Qualifier.MultiImpl initialWhens defaultImpl ->
                    typeCheckMultiImplementation context untypedDef initialWhens defaultImpl


cleanContext : Context -> Context
cleanContext ctx =
    { ctx
        | stackEffects = []
        , boundGenerics = Dict.empty
        , boundStackRanges = Dict.empty
    }


typeCheckSoloImplementation : Context -> Qualifier.WordDefinition -> List Qualifier.Node -> Context
typeCheckSoloImplementation context untypedDef impl =
    let
        ( inferredType, newContext ) =
            typeCheckImplementation untypedDef impl (cleanContext context)

        finalContext =
            { newContext
                | typedWords =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , type_ =
                            untypedDef.metadata.type_
                                |> TypeSignature.toMaybe
                                |> Maybe.withDefault inferredType
                        , metadata = untypedDef.metadata
                        , implementation = SoloImpl (List.map (untypedToTypedNode newContext) impl)
                        }
                        newContext.typedWords
            }
    in
    verifyTypeSignature inferredType untypedDef finalContext
        |> cleanContext


typeCheckMultiImplementation :
    Context
    -> Qualifier.WordDefinition
    -> List ( Qualifier.TypeMatch, List Qualifier.Node )
    -> List Qualifier.Node
    -> Context
typeCheckMultiImplementation context untypedDef initialWhens defaultImpl =
    let
        whens =
            case defaultImpl of
                [] ->
                    initialWhens

                _ ->
                    let
                        ( inferredDefaultType, _ ) =
                            typeCheckImplementation untypedDef defaultImpl (cleanContext context)
                    in
                    case inferredDefaultType.input of
                        [] ->
                            Debug.todo "Default impl doesn't have an input argument"

                        firstType :: _ ->
                            ( Qualifier.TypeMatch SourceLocation.emptyRange firstType [], defaultImpl ) :: initialWhens

        ( inferredWhenTypes, newContext ) =
            List.foldr (inferWhenTypes untypedDef) ( [], context ) whens
                |> Tuple.mapFirst (Debug.log "a")
                |> Tuple.mapFirst normalizeWhenTypes
                |> (\( wts, ctx ) -> simplifyWhenWordTypes wts ctx)
                |> Tuple.mapFirst equalizeWhenTypes
                |> Tuple.mapFirst (\whenTypes -> List.map (constrainGenerics untypedDef.metadata.type_) whenTypes)

        whensAreConsistent =
            inferredWhenTypes
                |> List.map2 Tuple.pair (List.map Tuple.first whens)
                |> List.all typeCheckWhen

        typeCheckWhen ( Qualifier.TypeMatch _ forType _, inf ) =
            case inf.input of
                firstInput :: _ ->
                    Debug.log "?" (Type.genericlyCompatible (Debug.log "first" firstInput) (Debug.log "forType" forType))

                [] ->
                    False

        whensAreCompatible =
            inferredWhenTypes
                |> List.map (stripFirstInput >> countOutput)
                |> areAllEqual

        stripFirstInput inf =
            { inf | input = List.drop 1 inf.input }

        countOutput wordType =
            ( wordType.input, List.length wordType.output )

        areAllEqual ls =
            case ls of
                [] ->
                    True

                ( fTypes, fCnt ) :: rest ->
                    List.all
                        (\( nTypes, nCnt ) ->
                            (fCnt == nCnt)
                                && compatibleTypeList fTypes nTypes
                        )
                        rest

        compatibleTypeList aLs bLs =
            List.map2 Type.genericlyCompatible aLs bLs
                |> List.all identity

        inferredType =
            List.head (Debug.log "infWhens" inferredWhenTypes)
                |> Maybe.withDefault { input = [], output = [] }
                |> replaceFirstType (unionOfTypeMatches whens)
                |> joinOutputs (List.map .output inferredWhenTypes)
                |> Debug.log "inf"

        finalContext =
            { newContext
                | typedWords =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , type_ =
                            TypeSignature.toMaybe untypedDef.metadata.type_
                                |> Maybe.withDefault inferredType
                                |> Debug.log "type"
                        , metadata = untypedDef.metadata
                        , implementation =
                            MultiImpl
                                (List.map (Tuple.mapBoth mapTypeMatch (List.map (untypedToTypedNode newContext))) initialWhens)
                                (List.map (untypedToTypedNode newContext) defaultImpl)
                        }
                        newContext.typedWords
                , errors =
                    if whensAreConsistent && whensAreCompatible then
                        newContext.errors

                    else
                        let
                            error =
                                InconsistentWhens
                                    (Maybe.withDefault SourceLocation.emptyRange untypedDef.metadata.sourceLocationRange)
                                    untypedDef.name
                        in
                        error :: newContext.errors
            }
    in
    verifyTypeSignature inferredType untypedDef finalContext
        |> cleanContext


inferWhenTypes :
    Qualifier.WordDefinition
    -> ( Qualifier.TypeMatch, List Qualifier.Node )
    -> ( List WordType, Context )
    -> ( List WordType, Context )
inferWhenTypes untypedDef ( _, im ) ( infs, ctx ) =
    let
        ( inf, newCtx ) =
            typeCheckImplementation untypedDef im (cleanContext ctx)
    in
    ( inf :: infs, newCtx )


normalizeWhenTypes : List WordType -> List WordType
normalizeWhenTypes whenTypes =
    let
        maybeLongestInputWhenType =
            List.sortBy (.input >> List.length) whenTypes
                |> List.reverse
                |> List.head

        matchInputLength toMatch wordType =
            let
                diff =
                    List.length toMatch.input - List.length wordType.input

                padding =
                    List.take diff toMatch.input
                        |> List.map padGeneric
            in
            case padding of
                [] ->
                    wordType

                elements ->
                    { wordType
                        | input = elements ++ wordType.input
                        , output = elements ++ wordType.output
                    }

        padGeneric t =
            case t of
                Type.Generic val ->
                    Type.Generic ("*" ++ val)

                _ ->
                    t
    in
    case maybeLongestInputWhenType of
        Just longestInputWT ->
            List.map (matchInputLength longestInputWT) whenTypes

        Nothing ->
            whenTypes


simplifyWhenWordTypes : List WordType -> Context -> ( List WordType, Context )
simplifyWhenWordTypes wordTypes context =
    ( List.map (\wt -> Tuple.second (simplifyWordType ( context, wt ))) wordTypes
    , context
    )


equalizeWhenTypes : List WordType -> List WordType
equalizeWhenTypes wordTypes =
    equalizeWhenTypesHelper wordTypes Dict.empty []


equalizeWhenTypesHelper : List WordType -> Dict String Type -> List WordType -> List WordType
equalizeWhenTypesHelper types remappedGenerics acc =
    case types of
        [] ->
            List.reverse acc

        lastType :: [] ->
            List.reverse (lastType :: acc)

        firstType :: secondType :: remaining ->
            let
                constrainAndZip lhs rhs =
                    case ( lhs, rhs ) of
                        ( Type.Generic _, Type.Generic _ ) ->
                            ( lhs, rhs )

                        ( Type.Generic _, other ) ->
                            ( other, other )

                        ( other, Type.Generic _ ) ->
                            ( other, other )

                        _ ->
                            ( lhs, rhs )

                unzip ( left, right ) ( leftAcc, rightAcc ) =
                    ( left :: leftAcc, right :: rightAcc )

                constrainedInputs =
                    List.map2 constrainAndZip firstType.input secondType.input

                constrainedOutputs =
                    List.map2 constrainAndZip firstType.output secondType.output

                ( unzippedFirstInputs, unzippedSecondInputs ) =
                    List.foldr unzip ( [], [] ) constrainedInputs

                ( unzippedFirstOutputs, unzippedSecondOutputs ) =
                    List.foldr unzip ( [], [] ) constrainedOutputs

                newFirstType =
                    { input = unzippedFirstInputs
                    , output = unzippedFirstOutputs
                    }

                newSecondType =
                    { input = unzippedSecondInputs
                    , output = unzippedSecondOutputs
                    }
            in
            equalizeWhenTypesHelper
                remaining
                remappedGenerics
                (newSecondType :: newFirstType :: acc)


unionOfTypeMatches : List ( Qualifier.TypeMatch, a ) -> Type
unionOfTypeMatches whenBranches =
    let
        uniqueTypes =
            whenBranches
                |> List.map (Tuple.first >> extractTypeFromTypeMatch)
                |> List.gatherEquals
                |> List.map Tuple.first
    in
    case uniqueTypes of
        [ singleType ] ->
            singleType

        _ ->
            Type.Union uniqueTypes


replaceFirstType : Type -> WordType -> WordType
replaceFirstType with inf =
    case inf.input of
        _ :: rem ->
            { inf | input = with :: rem }

        _ ->
            inf


joinOutputs : List (List Type) -> WordType -> WordType
joinOutputs outputs result =
    case outputs of
        first :: second :: rest ->
            let
                joined =
                    List.map2 unionize first second

                unionize lhs rhs =
                    case ( lhs, rhs ) of
                        _ ->
                            if lhs == rhs then
                                lhs

                            else
                                Type.Union [ lhs, rhs ]
            in
            joinOutputs (joined :: rest) result

        joined :: [] ->
            { result | output = joined }

        _ ->
            result


constrainGenerics : TypeSignature -> WordType -> WordType
constrainGenerics typeSignature inferredType =
    case TypeSignature.toMaybe typeSignature of
        Nothing ->
            inferredType

        Just annotatedType ->
            let
                ( remappedGenerics, constrainedInputs ) =
                    constrainGenericsHelper Dict.empty annotatedType.input inferredType.input []

                ( _, constrainedOutputs ) =
                    constrainGenericsHelper remappedGenerics annotatedType.output inferredType.output []
            in
            { input = constrainedInputs
            , output = constrainedOutputs
            }


constrainGenericsHelper : Dict String String -> List Type -> List Type -> List Type -> ( Dict String String, List Type )
constrainGenericsHelper remappedGenerics annotated inferred acc =
    case ( annotated, inferred ) of
        ( [], _ ) ->
            ( remappedGenerics, List.reverse acc )

        ( _, [] ) ->
            ( remappedGenerics, List.reverse acc )

        ( (Type.Generic annGen) :: annotatedRest, ((Type.Generic infGen) as inferredEl) :: inferredRest ) ->
            if annGen == infGen then
                constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)

            else
                case Dict.get infGen remappedGenerics of
                    Just val ->
                        constrainGenericsHelper remappedGenerics annotatedRest inferredRest (Type.Generic val :: acc)

                    Nothing ->
                        constrainGenericsHelper
                            (Dict.insert infGen annGen remappedGenerics)
                            annotatedRest
                            inferredRest
                            (Type.Generic annGen :: acc)

        ( _ :: annotatedRest, inferredEl :: inferredRest ) ->
            constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)


typeCheckImplementation : Qualifier.WordDefinition -> List Qualifier.Node -> Context -> ( WordType, Context )
typeCheckImplementation untypedDef impl context =
    let
        contextWithCall =
            { context | callStack = Set.insert untypedDef.name context.callStack }

        ( _, contextWithStackEffects ) =
            List.foldl
                (\node ( idx, ctx ) -> ( idx + 1, typeCheckNode idx node ctx ))
                ( 0, contextWithCall )
                impl

        contextWithoutCall =
            { contextWithStackEffects | callStack = Set.remove untypedDef.name contextWithStackEffects.callStack }
    in
    wordTypeFromStackEffects untypedDef contextWithoutCall
        |> simplifyWordType
        |> (\( a, b ) -> ( b, a ))


extractTypeFromTypeMatch : Qualifier.TypeMatch -> Type
extractTypeFromTypeMatch (Qualifier.TypeMatch _ t_ _) =
    t_


mapTypeMatch : Qualifier.TypeMatch -> TypeMatch
mapTypeMatch (Qualifier.TypeMatch range type_ cond) =
    TypeMatch range type_ (List.map mapTypeMatchValue cond)


mapTypeMatchValue : ( String, Qualifier.TypeMatchValue ) -> ( String, TypeMatchValue )
mapTypeMatchValue ( fieldName, value ) =
    case value of
        Qualifier.LiteralInt val ->
            ( fieldName, LiteralInt val )

        Qualifier.LiteralType val ->
            ( fieldName, LiteralType val )

        Qualifier.RecursiveMatch val ->
            ( fieldName, RecursiveMatch (mapTypeMatch val) )


verifyTypeSignature : WordType -> Qualifier.WordDefinition -> Context -> Context
verifyTypeSignature inferredType untypedDef context =
    case TypeSignature.toMaybe untypedDef.metadata.type_ of
        Just annotatedType ->
            let
                ( _, simplifiedAnnotatedType ) =
                    simplifyWordType ( context, annotatedType )
            in
            if not <| Type.compatibleWords simplifiedAnnotatedType inferredType then
                let
                    range =
                        untypedDef.metadata.sourceLocationRange
                            |> Maybe.withDefault SourceLocation.emptyRange

                    problem =
                        TypeError range untypedDef.name simplifiedAnnotatedType inferredType
                in
                { context | errors = problem :: context.errors }

            else
                context

        Nothing ->
            context


typeCheckNode : Int -> Qualifier.Node -> Context -> Context
typeCheckNode idx node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = ctx.stackEffects ++ List.map (tagGenericEffect idx) effects }
    in
    case node of
        Qualifier.Integer _ _ ->
            addStackEffect context [ Push Type.Int ]

        Qualifier.Word _ name ->
            case Dict.get name context.typedWords of
                Just def ->
                    addStackEffect context <| wordTypeToStackEffects def.type_

                Nothing ->
                    case Dict.get name context.untypedWords of
                        Nothing ->
                            Debug.todo "inconcievable!"

                        Just untypedDef ->
                            if Set.member name context.callStack then
                                -- recursive definition!
                                case TypeSignature.toMaybe untypedDef.metadata.type_ of
                                    Just annotatedType ->
                                        addStackEffect context <| wordTypeToStackEffects annotatedType

                                    Nothing ->
                                        let
                                            problem =
                                                MissingTypeAnnotationInRecursiveCallStack
                                                    (Maybe.withDefault SourceLocation.emptyRange untypedDef.metadata.sourceLocationRange)
                                                    untypedDef.name
                                        in
                                        { context | errors = problem :: context.errors }

                            else
                                let
                                    contextWithTypedDef =
                                        typeCheckDefinition untypedDef context

                                    newContext =
                                        { contextWithTypedDef | stackEffects = context.stackEffects }
                                in
                                case Dict.get name newContext.typedWords of
                                    Nothing ->
                                        Debug.todo "inconcievable!"

                                    Just def ->
                                        addStackEffect newContext <| wordTypeToStackEffects def.type_

        Qualifier.WordRef loc ref ->
            let
                contextAfterWordCheck =
                    typeCheckNode idx (Qualifier.Word loc ref) context
            in
            case Dict.get ref contextAfterWordCheck.typedWords of
                Just def ->
                    addStackEffect contextAfterWordCheck <|
                        [ Push <| Type.Quotation def.type_ ]

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.ConstructType typeName ->
            case Dict.get typeName context.types of
                Just (CustomTypeDef _ _ _ members) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter Type.isGeneric memberTypes

                        typeInQuestion =
                            case genericMembers of
                                [] ->
                                    Type.Custom typeName

                                _ ->
                                    Type.CustomGeneric typeName genericMembers
                    in
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = memberTypes
                            , output = [ typeInQuestion ]
                            }

                other ->
                    Debug.todo ("inconcievable: " ++ typeName ++ ": " ++ Debug.toString other)

        Qualifier.SetMember typeName memberName ->
            case
                ( Dict.get typeName context.types
                , getMemberType context.types typeName memberName
                )
            of
                ( Just (CustomTypeDef _ _ _ members), Just memberType ) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter Type.isGeneric memberTypes

                        typeInQuestion =
                            case genericMembers of
                                [] ->
                                    Type.Custom typeName

                                _ ->
                                    Type.CustomGeneric typeName genericMembers
                    in
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = [ typeInQuestion, memberType ]
                            , output = [ typeInQuestion ]
                            }

                other ->
                    Debug.todo ("inconcievable! " ++ Debug.toString other)

        Qualifier.GetMember typeName memberName ->
            case
                ( Dict.get typeName context.types
                , getMemberType context.types typeName memberName
                )
            of
                ( Just (CustomTypeDef _ _ _ members), Just memberType ) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter Type.isGeneric memberTypes

                        typeInQuestion =
                            case genericMembers of
                                [] ->
                                    Type.Custom typeName

                                _ ->
                                    Type.CustomGeneric typeName genericMembers
                    in
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = [ typeInQuestion ]
                            , output = [ memberType ]
                            }

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.Builtin _ builtin ->
            addStackEffect context <| wordTypeToStackEffects <| Builtin.wordType builtin


tagGenericEffect : Int -> StackEffect -> StackEffect
tagGenericEffect idx effect =
    case effect of
        Push type_ ->
            Push <| tagGeneric idx type_

        Pop type_ ->
            Pop <| tagGeneric idx type_


tagGeneric : Int -> Type -> Type
tagGeneric idx type_ =
    case type_ of
        Type.Generic genName ->
            Type.Generic (genName ++ String.fromInt idx)

        Type.CustomGeneric name generics ->
            Type.CustomGeneric name (List.map (tagGeneric idx) generics)

        Type.Union members ->
            Type.Union (List.map (tagGeneric idx) members)

        _ ->
            type_


wordTypeToStackEffects : WordType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Pop (List.reverse wordType.input)
        ++ List.map Push wordType.output


wordTypeFromStackEffects : Qualifier.WordDefinition -> Context -> ( Context, WordType )
wordTypeFromStackEffects untypedDef context =
    wordTypeFromStackEffectsHelper untypedDef context.stackEffects ( context, { input = [], output = [] } )


wordTypeFromStackEffectsHelper : Qualifier.WordDefinition -> List StackEffect -> ( Context, WordType ) -> ( Context, WordType )
wordTypeFromStackEffectsHelper untypedDef effects ( context, wordType ) =
    let
        problem expected actual =
            UnexpectedType
                (Maybe.withDefault SourceLocation.emptyRange untypedDef.metadata.sourceLocationRange)
                untypedDef.name
                expected
                actual
    in
    case effects of
        [] ->
            ( context
            , { wordType
                | input = wordType.input
                , output = List.reverse wordType.output
              }
            )

        (Pop ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just needToPop ->
                    wordTypeFromStackEffectsHelper untypedDef (List.map Pop needToPop ++ remainingEffects) ( context, wordType )

                Nothing ->
                    case wordType.output of
                        [] ->
                            wordTypeFromStackEffectsHelper untypedDef remainingEffects <|
                                ( context, { wordType | input = type_ :: wordType.input } )

                        availableType :: remainingOutput ->
                            if availableType /= type_ then
                                ( { context | errors = problem type_ availableType :: context.errors }, wordType )

                            else
                                ( context, { wordType | output = remainingOutput } )

        (Pop type_) :: remainingEffects ->
            case wordType.output of
                [] ->
                    wordTypeFromStackEffectsHelper untypedDef remainingEffects <|
                        ( context, { wordType | input = type_ :: wordType.input } )

                availableType :: remainingOutput ->
                    let
                        ( newContext, compatible ) =
                            compatibleTypes context availableType type_
                    in
                    if not compatible then
                        ( { newContext | errors = problem type_ availableType :: context.errors }, wordType )

                    else
                        wordTypeFromStackEffectsHelper untypedDef remainingEffects <|
                            ( newContext, { wordType | output = remainingOutput } )

        (Push ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just range ->
                    wordTypeFromStackEffectsHelper untypedDef
                        (List.map Push range ++ remainingEffects)
                        ( context, wordType )

                Nothing ->
                    wordTypeFromStackEffectsHelper untypedDef remainingEffects <|
                        ( context, { wordType | output = type_ :: wordType.output } )

        (Push type_) :: remainingEffects ->
            wordTypeFromStackEffectsHelper untypedDef remainingEffects <|
                ( context, { wordType | output = type_ :: wordType.output } )


compatibleTypes : Context -> Type -> Type -> ( Context, Bool )
compatibleTypes context typeA typeB =
    case ( getGenericBinding context typeA, getGenericBinding context typeB ) of
        -- A is unbound
        ( Nothing, Just boundB ) ->
            ( bindGeneric typeA boundB context, True )

        -- B is unbound
        ( Just boundA, Nothing ) ->
            ( bindGeneric typeB boundA context, True )

        -- Both are unbound
        ( Nothing, Nothing ) ->
            ( bindGeneric typeA typeB context
                |> bindGeneric typeB typeA
            , True
            )

        -- Both types are either a resolved type or bound generic
        ( Just boundA, Just boundB ) ->
            if boundA == boundB then
                ( context, True )

            else
                case ( boundA, boundB ) of
                    ( Type.Union leftUnion, Type.Union rightUnion ) ->
                        -- TODO: Requires unions to be sorted in same order
                        let
                            lengthTest =
                                List.length leftUnion == List.length rightUnion

                            ( newContext, allMembersTest ) =
                                List.map2 Tuple.pair leftUnion rightUnion
                                    |> List.foldl foldHelper ( context, True )

                            foldHelper ( lType, rType ) ( ctx, currValue ) =
                                if not currValue then
                                    ( ctx, currValue )

                                else
                                    compatibleTypes ctx lType rType
                        in
                        ( newContext
                        , lengthTest && allMembersTest
                        )

                    ( Type.Union _, _ ) ->
                        -- Cannot go from union to concrete type
                        ( context, False )

                    ( lhsType, Type.Union unionTypes ) ->
                        List.map (compatibleTypes context lhsType) unionTypes
                            |> List.find Tuple.second
                            |> Maybe.withDefault ( context, False )

                    ( Type.CustomGeneric lName lMembers, Type.CustomGeneric rName rMembers ) ->
                        let
                            ( updatedContext, compatible ) =
                                List.foldl foldHelper ( context, True ) members

                            members =
                                List.map2 Tuple.pair lMembers rMembers

                            foldHelper ( lType, rType ) (( currCtx, isCompatible ) as acc) =
                                if not isCompatible then
                                    acc

                                else
                                    compatibleTypes currCtx lType rType
                        in
                        if lName == rName && compatible then
                            ( updatedContext, True )

                        else
                            ( context, False )

                    ( Type.Quotation lhs, Type.Quotation rhs ) ->
                        let
                            boundRanges =
                                Dict.empty
                                    |> bindStackRange context lhs.input rhs.input
                                    |> bindStackRange context lhs.output rhs.output

                            actualInputRequirement =
                                replaceStackRange boundRanges rhs.input

                            actualOutputRequirement =
                                replaceStackRange boundRanges rhs.output
                        in
                        ( { context
                            | boundStackRanges = Dict.union context.boundStackRanges boundRanges
                          }
                        , lhs.input == actualInputRequirement && lhs.output == actualOutputRequirement
                        )

                    _ ->
                        ( context, False )


getGenericBinding : Context -> Type -> Maybe Type
getGenericBinding context type_ =
    case type_ of
        Type.Generic genericId ->
            case Dict.get genericId context.boundGenerics of
                Just (Type.Generic nextGenericId) ->
                    case Dict.get nextGenericId context.boundGenerics of
                        Just ((Type.Generic cycleCheckId) as anotherGenericType) ->
                            if cycleCheckId == genericId then
                                Nothing

                            else
                                getGenericBinding context anotherGenericType

                        otherwise ->
                            otherwise

                otherwise ->
                    otherwise

        _ ->
            Just type_


bindGeneric : Type -> Type -> Context -> Context
bindGeneric toBind target context =
    case toBind of
        Type.Generic name ->
            { context | boundGenerics = Dict.insert name target context.boundGenerics }

        _ ->
            context


bindStackRange : Context -> List Type -> List Type -> Dict String (List Type) -> Dict String (List Type)
bindStackRange context actual expected bound =
    let
        rangeUpdater existing newType =
            case existing of
                Just vals ->
                    Just <| vals ++ [ newType ]

                Nothing ->
                    Just [ newType ]
    in
    case ( actual, expected ) of
        ( [], _ ) ->
            bound

        ( _, [] ) ->
            bound

        ( afirst :: arest, (Type.StackRange rangeName) :: [] ) ->
            let
                newBound =
                    Dict.update rangeName (\existing -> rangeUpdater existing afirst) bound
            in
            bindStackRange context arest expected newBound

        ( afirst :: arest, bfirst :: brest ) ->
            let
                ( newContext, compatible ) =
                    compatibleTypes context afirst bfirst
            in
            if compatible then
                bindStackRange newContext arest brest bound

            else
                bound


replaceStackRange : Dict String (List Type) -> List Type -> List Type
replaceStackRange boundRanges types =
    List.concatMap
        (\t ->
            case t of
                Type.StackRange rangeName ->
                    Dict.get rangeName boundRanges
                        |> Maybe.withDefault []

                otherwise ->
                    [ otherwise ]
        )
        types


simplifyWordType : ( Context, WordType ) -> ( Context, WordType )
simplifyWordType ( context, wordType ) =
    let
        oldSignature =
            wordType.input ++ wordType.output

        inputLength =
            List.length wordType.input

        aliases =
            oldSignature
                |> List.filterMap Type.genericName
                -- remove duplicates
                |> (Set.fromList >> Set.toList)
                |> List.map (findAliases context)
                |> List.foldl reverseLookup Dict.empty

        newSignature =
            List.map reduceGenericName oldSignature
                |> List.foldl renameGenerics ( 'a', Dict.empty, [] )
                |> (\( _, _, ns ) -> ns)
                |> List.reverse

        reduceGenericName type_ =
            case type_ of
                Type.Generic genName ->
                    case getGenericBinding context type_ of
                        Just boundType ->
                            boundType

                        Nothing ->
                            case Dict.get genName aliases of
                                Just actualName ->
                                    Type.Generic actualName

                                Nothing ->
                                    type_

                Type.Union members ->
                    Type.Union (List.map reduceGenericName members)

                Type.CustomGeneric name members ->
                    Type.CustomGeneric name (List.map reduceGenericName members)

                _ ->
                    type_

        renameGenerics type_ ( nextId, seenGenerics, acc ) =
            case type_ of
                Type.Generic genName ->
                    case Dict.get genName seenGenerics of
                        Just newName ->
                            ( nextId, seenGenerics, Type.Generic newName :: acc )

                        Nothing ->
                            let
                                newName =
                                    String.fromChar nextId
                            in
                            ( nextId
                                |> Char.toCode
                                |> (+) 1
                                |> Char.fromCode
                            , Dict.insert genName newName seenGenerics
                            , Type.Generic newName :: acc
                            )

                Type.Union members ->
                    let
                        ( newNextId, newSeenGenerics, newMembers ) =
                            List.foldr renameGenerics ( nextId, seenGenerics, [] ) members
                    in
                    ( newNextId, newSeenGenerics, Type.Union newMembers :: acc )

                Type.CustomGeneric name members ->
                    let
                        ( newNextId, newSeenGenerics, newMembers ) =
                            List.foldr renameGenerics ( nextId, seenGenerics, [] ) members
                    in
                    ( newNextId, newSeenGenerics, Type.CustomGeneric name newMembers :: acc )

                _ ->
                    ( nextId, seenGenerics, type_ :: acc )
    in
    ( context
    , { input = List.take inputLength newSignature
      , output = List.drop inputLength newSignature
      }
    )


findAliases : Context -> String -> ( String, List String )
findAliases context generic =
    ( generic
    , context.boundGenerics
        |> Dict.keys
        |> List.filterMap (\key -> isAliasOf context Set.empty generic key key)
    )


isAliasOf : Context -> Set String -> String -> String -> String -> Maybe String
isAliasOf context visitedKeys targetKey topKey currentKey =
    case Dict.get currentKey context.boundGenerics of
        Just (Type.Generic genericKey) ->
            if Set.member genericKey visitedKeys then
                Nothing

            else if genericKey == targetKey then
                Just topKey

            else
                isAliasOf context (Set.insert currentKey visitedKeys) targetKey topKey genericKey

        _ ->
            Nothing


reverseLookup : ( String, List String ) -> Dict String String -> Dict String String
reverseLookup ( name, aliases ) acc =
    let
        targetName =
            Dict.get name acc
                |> Maybe.withDefault name
    in
    aliases
        |> List.filter (\a -> not <| Dict.member a acc)
        |> List.filter (\a -> a /= targetName)
        |> List.foldl (\alias newAcc -> Dict.insert alias targetName newAcc) acc


untypedToTypedNode : Context -> Qualifier.Node -> AstNode
untypedToTypedNode context untypedNode =
    case untypedNode of
        Qualifier.Integer range num ->
            IntLiteral range num

        Qualifier.Word range name ->
            case Dict.get name context.typedWords of
                Just def ->
                    Word range name def.type_

                Nothing ->
                    Dict.get name context.untypedWords
                        |> Maybe.andThen (.metadata >> .type_ >> TypeSignature.toMaybe)
                        |> Maybe.withDefault { input = [], output = [] }
                        |> Word range name

        Qualifier.WordRef range ref ->
            WordRef range ref

        Qualifier.ConstructType typeName ->
            ConstructType typeName

        Qualifier.SetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    SetMember typeName memberName memberType

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.GetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    GetMember typeName memberName memberType

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.Builtin range builtin ->
            Builtin range builtin


getMemberType : Dict String TypeDefinition -> String -> String -> Maybe Type
getMemberType typeDict typeName memberName =
    case Dict.get typeName typeDict of
        Just (CustomTypeDef _ _ _ members) ->
            List.find (\( name, _ ) -> name == memberName) members
                |> Maybe.map Tuple.second

        _ ->
            Nothing


typeDefName : TypeDefinition -> String
typeDefName typeDef =
    case typeDef of
        CustomTypeDef name _ _ _ ->
            name

        UnionTypeDef name _ _ _ ->
            name
