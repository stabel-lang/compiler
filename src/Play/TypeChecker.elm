module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Qualifier as Qualifier
import Set exposing (Set)


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type TypeDefinition
    = CustomTypeDef String (List ( String, Type ))
    | UnionTypeDef String (List Type)


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
    = TypeMatch Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type AstNode
    = IntLiteral Int
    | Word String WordType
    | WordRef String
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type
    | Builtin Builtin


type alias Context =
    { types : Dict String TypeDefinition
    , typedWords : Dict String WordDefinition
    , untypedWords : Dict String Qualifier.WordDefinition
    , stackEffects : List StackEffect
    , boundGenerics : Dict String Type
    , boundStackRanges : Dict String (List Type)
    , errors : List ()
    }


type StackEffect
    = Push Type
    | Pop Type


initContext : Qualifier.AST -> Context
initContext ast =
    let
        concreteTypes =
            ast.types
                |> Dict.values
                |> List.map
                    (\t ->
                        case t of
                            Qualifier.CustomTypeDef name _ members ->
                                CustomTypeDef name members

                            Qualifier.UnionTypeDef name _ memberTypes ->
                                UnionTypeDef name memberTypes
                    )
                |> Dict.fromListBy typeDefName
    in
    { types = concreteTypes
    , typedWords = Dict.empty
    , untypedWords = ast.words
    , stackEffects = []
    , boundGenerics = Dict.empty
    , boundStackRanges = Dict.empty
    , errors = []
    }


typeCheck : Qualifier.AST -> Result () AST
typeCheck ast =
    typeCheckHelper (initContext ast) ast


typeCheckHelper : Context -> Qualifier.AST -> Result () AST
typeCheckHelper context ast =
    let
        updatedContext =
            ast.words
                |> Dict.values
                |> List.foldl typeCheckDefinition context

        concreteTypes =
            ast.types
                |> Dict.values
                |> List.map
                    (\t ->
                        case t of
                            Qualifier.CustomTypeDef name _ members ->
                                CustomTypeDef name members

                            Qualifier.UnionTypeDef name _ memberTypes ->
                                UnionTypeDef name memberTypes
                    )
                |> Dict.fromListBy typeDefName
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = concreteTypes
            , words = updatedContext.typedWords
            }

    else
        Err ()


typeCheckDefinition : Qualifier.WordDefinition -> Context -> Context
typeCheckDefinition untypedDef context =
    let
        cleanContext ctx =
            { ctx
                | stackEffects = []
                , boundGenerics = Dict.empty
                , boundStackRanges = Dict.empty
            }
    in
    case Dict.get untypedDef.name context.typedWords of
        Just _ ->
            cleanContext context

        Nothing ->
            case untypedDef.implementation of
                Qualifier.SoloImpl impl ->
                    let
                        ( inferredType, newContext ) =
                            typeCheckImplementation untypedDef impl (cleanContext context)

                        finalContext =
                            { newContext
                                | typedWords =
                                    Dict.insert untypedDef.name
                                        { name = untypedDef.name
                                        , type_ =
                                            case untypedDef.metadata.type_ of
                                                Just annotatedType ->
                                                    annotatedType

                                                Nothing ->
                                                    inferredType
                                        , metadata = untypedDef.metadata
                                        , implementation = SoloImpl (List.map (untypedToTypedNode newContext) impl)
                                        }
                                        newContext.typedWords
                            }
                    in
                    verifyTypeSignature inferredType untypedDef finalContext
                        |> cleanContext

                Qualifier.MultiImpl initialWhens defaultImpl ->
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
                                            ( Qualifier.TypeMatch firstType [], defaultImpl ) :: initialWhens

                        inferWhenTypes ( _, im ) ( infs, ctx ) =
                            let
                                ( inf, newCtx ) =
                                    typeCheckImplementation untypedDef im (cleanContext ctx)
                            in
                            ( inf :: infs, newCtx )

                        ( inferredWhenTypes, newContext ) =
                            List.foldr inferWhenTypes ( [], context ) whens

                        whensAreConsistent =
                            inferredWhenTypes
                                |> List.map2 Tuple.pair (List.map Tuple.first whens)
                                |> List.map typeCheckWhen
                                |> List.all (\( b, _, _ ) -> b)

                        whensAreCompatible =
                            inferredWhenTypes
                                |> List.map stripFirstInput
                                |> List.map countOutput
                                |> areAllEqual

                        typeCheckWhen ( Qualifier.TypeMatch forType _, inf ) =
                            case inf.input of
                                firstInput :: _ ->
                                    let
                                        compatible =
                                            case firstInput of
                                                Type.Generic _ ->
                                                    True

                                                _ ->
                                                    firstInput == forType
                                    in
                                    ( compatible, forType, inf )

                                [] ->
                                    ( False, forType, inf )

                        stripFirstInput inf =
                            case inf.input of
                                _ :: rem ->
                                    { inf | input = rem }

                                _ ->
                                    inf

                        countOutput wordType =
                            ( wordType.input, List.length wordType.output )

                        areAllEqual ls =
                            case ls of
                                [] ->
                                    True

                                first :: rest ->
                                    List.all ((==) first) rest

                        inferredType =
                            List.head inferredWhenTypes
                                |> Maybe.withDefault { input = [], output = [] }
                                |> replaceFirstType (Type.Union (List.map (Tuple.first >> extractType) whens))
                                |> joinOutputs (List.map .output inferredWhenTypes)

                        replaceFirstType with inf =
                            case inf.input of
                                _ :: rem ->
                                    { inf | input = with :: rem }

                                _ ->
                                    inf

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

                        extractType (Qualifier.TypeMatch t_ _) =
                            t_

                        mapTypeMatch (Qualifier.TypeMatch type_ cond) =
                            TypeMatch type_ (List.map mapTypeMatchValue cond)

                        mapTypeMatchValue ( fieldName, value ) =
                            case value of
                                Qualifier.LiteralInt val ->
                                    ( fieldName, LiteralInt val )

                                Qualifier.LiteralType val ->
                                    ( fieldName, LiteralType val )

                                Qualifier.RecursiveMatch val ->
                                    ( fieldName, RecursiveMatch (mapTypeMatch val) )

                        finalContext =
                            { newContext
                                | typedWords =
                                    Dict.insert untypedDef.name
                                        { name = untypedDef.name
                                        , type_ =
                                            case untypedDef.metadata.type_ of
                                                Just annotatedType ->
                                                    { annotatedType
                                                        | input = List.map (resolveUnion newContext) annotatedType.input
                                                        , output = List.map (resolveUnion newContext) annotatedType.output
                                                    }

                                                Nothing ->
                                                    inferredType
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
                                        () :: newContext.errors
                            }
                    in
                    verifyTypeSignature inferredType untypedDef finalContext
                        |> cleanContext


resolveUnion : Context -> Type -> Type
resolveUnion context type_ =
    case type_ of
        Type.Custom typeName ->
            case Dict.get typeName context.types of
                Just (UnionTypeDef _ members) ->
                    Type.Union members

                _ ->
                    type_

        _ ->
            type_


typeCheckImplementation : Qualifier.WordDefinition -> List Qualifier.Node -> Context -> ( WordType, Context )
typeCheckImplementation untypedDef impl context =
    let
        ( _, contextWithStackEffects ) =
            List.foldl
                (\node ( idx, ctx ) -> ( idx + 1, typeCheckNode idx node ctx ))
                ( 0, context )
                impl
    in
    wordTypeFromStackEffects contextWithStackEffects
        |> simplifyWordType untypedDef.name
        |> (\( a, b ) -> ( b, a ))


verifyTypeSignature : WordType -> Qualifier.WordDefinition -> Context -> Context
verifyTypeSignature inferredType untypedDef context =
    case untypedDef.metadata.type_ of
        Just annotatedType ->
            let
                ( _, simplifiedAnnotatedType ) =
                    simplifyWordType untypedDef.name ( context, annotatedType )
            in
            if not <| compatibleWordTypes simplifiedAnnotatedType inferredType context then
                { context | errors = () :: context.errors }

            else
                context

        Nothing ->
            context


compatibleWordTypes : WordType -> WordType -> Context -> Bool
compatibleWordTypes annotated inferred context =
    let
        annotatedInputTypes =
            List.map (resolveUnion context) annotated.input

        inferredInputTypes =
            List.map (resolveUnion context) inferred.input

        annotatedOutputTypes =
            List.map (resolveUnion context) annotated.output

        inferredOutputTypes =
            List.map (resolveUnion context) inferred.output

        ( inputRangeDict, inputsCompatible ) =
            compareType_ annotatedInputTypes inferredInputTypes Dict.empty

        ( _, outputsCompatible ) =
            compareType_ annotatedOutputTypes inferredOutputTypes inputRangeDict
    in
    inputsCompatible && outputsCompatible


compareType_ : List Type -> List Type -> Dict String (List Type) -> ( Dict String (List Type), Bool )
compareType_ lhs rhs rangeDict =
    case ( lhs, rhs ) of
        ( [], [] ) ->
            ( rangeDict, True )

        ( (Type.StackRange lhsName) :: lhsRest, (Type.StackRange rhsName) :: rhsRest ) ->
            if lhsName == rhsName then
                compareType_ lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( (Type.StackRange _) :: [], [] ) ->
            ( rangeDict, True )

        ( [], (Type.StackRange _) :: [] ) ->
            ( rangeDict, True )

        ( lhsEl :: lhsRest, (Type.StackRange rangeName) :: [] ) ->
            compareType_ lhsRest rhs <|
                Dict.update rangeName
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault []
                            |> (\existing -> existing ++ [ lhsEl ])
                            |> Just
                    )
                    rangeDict

        ( lhsEl :: lhsRest, (Type.StackRange rangeName) :: rhsNext :: rhsRest ) ->
            if sameBaseType lhsEl rhsNext then
                compareType_ lhs (rhsNext :: rhsRest) rangeDict

            else
                compareType_ lhsRest rhs <|
                    Dict.update rangeName
                        (\maybeVal ->
                            maybeVal
                                |> Maybe.withDefault []
                                |> (\existing -> existing ++ [ lhsEl ])
                                |> Just
                        )
                        rangeDict

        ( (Type.Quotation lhsQuotType) :: lhsRest, (Type.Quotation rhsQuotType) :: rhsRest ) ->
            let
                lhsInputRangeApplied =
                    applyRangeDict rangeDict lhsQuotType.input

                lhsOutputRangeApplied =
                    applyRangeDict rangeDict lhsQuotType.output

                rhsInputRangeApplied =
                    applyRangeDict rangeDict rhsQuotType.input

                rhsOutputRangeApplied =
                    applyRangeDict rangeDict rhsQuotType.output

                applyRangeDict rd types =
                    List.concatMap
                        (\type_ ->
                            case type_ of
                                Type.StackRange rangeName ->
                                    case Dict.get rangeName rd of
                                        Just subst ->
                                            subst

                                        Nothing ->
                                            [ type_ ]

                                other ->
                                    [ other ]
                        )
                        types

                ( dictRangePostInputs, inputCompatible ) =
                    compareType_ lhsInputRangeApplied rhsInputRangeApplied rangeDict

                ( dictRangePostOutputs, outputCompatible ) =
                    compareType_ lhsOutputRangeApplied rhsOutputRangeApplied dictRangePostInputs
            in
            if inputCompatible && outputCompatible then
                compareType_ lhsRest rhsRest dictRangePostOutputs

            else
                ( dictRangePostOutputs, False )

        ( (Type.Generic _) :: lhsRest, _ :: rhsRest ) ->
            compareType_ lhsRest rhsRest rangeDict

        ( _ :: lhsRest, (Type.Generic _) :: rhsRest ) ->
            compareType_ lhsRest rhsRest rangeDict

        ( (Type.CustomGeneric lName lMembers) :: lhsRest, (Type.CustomGeneric rName rMembers) :: rhsRest ) ->
            let
                ( _, compatibleMembers ) =
                    compareType_ lMembers rMembers Dict.empty
            in
            if lName == rName && compatibleMembers then
                compareType_ lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( (Type.Union lMembers) :: lhsRest, (Type.Union rMembers) :: rhsRest ) ->
            let
                lSet =
                    Set.fromList (List.map typeAsStr lMembers)

                rSet =
                    Set.fromList (List.map typeAsStr rMembers)

                diff =
                    Set.diff rSet lSet
                        |> Set.toList
            in
            case diff of
                [] ->
                    compareType_ lhsRest rhsRest rangeDict

                [ oneDiff ] ->
                    -- Likely the default case
                    if String.endsWith "_Generic" oneDiff then
                        compareType_ lhsRest rhsRest rangeDict

                    else
                        ( rangeDict, False )

                _ ->
                    ( rangeDict, False )

        ( (Type.Union _) :: _, _ ) ->
            -- Cannot go from union to concrete type
            ( rangeDict, False )

        ( lhsEl :: lhsRest, (Type.Union rMembers) :: rhsRest ) ->
            let
                compatible =
                    rMembers
                        |> List.map typeAsStr
                        |> Set.fromList
                        |> Set.member (typeAsStr lhsEl)
            in
            if compatible then
                compareType_ lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( lhsEl :: lhsRest, rhsEl :: rhsRest ) ->
            if lhsEl == rhsEl then
                compareType_ lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        _ ->
            ( rangeDict, False )


sameBaseType : Type -> Type -> Bool
sameBaseType lhs rhs =
    if lhs == rhs then
        True

    else
        case ( lhs, rhs ) of
            ( Type.Quotation _, Type.Quotation _ ) ->
                True

            ( Type.Union _, Type.Union _ ) ->
                True

            _ ->
                False


typeAsStr : Type -> String
typeAsStr t =
    case t of
        Type.Int ->
            "Int"

        Type.Generic name ->
            name ++ "_Generic"

        Type.Custom name ->
            name ++ "_Custom"

        Type.CustomGeneric name _ ->
            name ++ "_Custom"

        Type.Union _ ->
            "Union"

        Type.Quotation _ ->
            "quot"

        Type.StackRange name ->
            name ++ "..."


typeCheckNode : Int -> Qualifier.Node -> Context -> Context
typeCheckNode idx node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = ctx.stackEffects ++ List.map tagGenericEffect effects }

        tagGenericEffect effect =
            case effect of
                Push type_ ->
                    Push <| tagGeneric type_

                Pop type_ ->
                    Pop <| tagGeneric type_

        tagGeneric type_ =
            case type_ of
                Type.Generic genName ->
                    Type.Generic (genName ++ String.fromInt idx)

                Type.CustomGeneric name generics ->
                    Type.CustomGeneric name (List.map tagGeneric generics)

                _ ->
                    type_
    in
    case node of
        Qualifier.Integer _ ->
            addStackEffect context [ Push Type.Int ]

        Qualifier.Word name ->
            case Dict.get name context.typedWords of
                Just def ->
                    addStackEffect context <| wordTypeToStackEffects def.type_

                Nothing ->
                    case Dict.get name context.untypedWords of
                        Nothing ->
                            Debug.todo "inconcievable!"

                        Just untypedDef ->
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

        Qualifier.WordRef ref ->
            let
                contextAfterWordCheck =
                    typeCheckNode idx (Qualifier.Word ref) context
            in
            case Dict.get ref contextAfterWordCheck.typedWords of
                Just def ->
                    addStackEffect contextAfterWordCheck <|
                        [ Push <| Type.Quotation def.type_ ]

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.ConstructType typeName ->
            case Dict.get typeName context.types of
                Just (CustomTypeDef _ members) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter isGeneric memberTypes

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
                ( Just (CustomTypeDef _ members), Just memberType ) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter isGeneric memberTypes

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

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.GetMember typeName memberName ->
            case
                ( Dict.get typeName context.types
                , getMemberType context.types typeName memberName
                )
            of
                ( Just (CustomTypeDef _ members), Just memberType ) ->
                    let
                        memberTypes =
                            List.map Tuple.second members

                        genericMembers =
                            List.filter isGeneric memberTypes

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

        Qualifier.Builtin builtin ->
            addStackEffect context <| wordTypeToStackEffects <| Builtin.wordType builtin


wordTypeToStackEffects : WordType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Pop (List.reverse wordType.input)
        ++ List.map Push wordType.output


wordTypeFromStackEffects : Context -> ( Context, WordType )
wordTypeFromStackEffects context =
    wordTypeFromStackEffectsHelper context.stackEffects ( context, { input = [], output = [] } )


wordTypeFromStackEffectsHelper : List StackEffect -> ( Context, WordType ) -> ( Context, WordType )
wordTypeFromStackEffectsHelper effects ( context, wordType ) =
    case effects of
        [] ->
            ( context
              -- TODO: Really need to get this list reverse madness figured out and solved properly
            , { wordType
                | input = wordType.input
                , output = List.reverse wordType.output
              }
            )

        (Pop ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just needToPop ->
                    wordTypeFromStackEffectsHelper (List.map Pop needToPop ++ remainingEffects) ( context, wordType )

                Nothing ->
                    case wordType.output of
                        [] ->
                            wordTypeFromStackEffectsHelper remainingEffects <|
                                ( context, { wordType | input = type_ :: wordType.input } )

                        availableType :: remainingOutput ->
                            if availableType /= Type.StackRange rangeName then
                                ( { context | errors = () :: context.errors }, wordType )

                            else
                                ( context, { wordType | output = remainingOutput } )

        (Pop type_) :: remainingEffects ->
            case wordType.output of
                [] ->
                    wordTypeFromStackEffectsHelper remainingEffects <|
                        ( context, { wordType | input = type_ :: wordType.input } )

                availableType :: remainingOutput ->
                    let
                        ( newContext, compatible ) =
                            compatibleTypes context availableType type_
                    in
                    if not compatible then
                        ( { newContext | errors = () :: context.errors }, wordType )

                    else
                        wordTypeFromStackEffectsHelper remainingEffects <|
                            ( newContext, { wordType | output = remainingOutput } )

        (Push ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just range ->
                    wordTypeFromStackEffectsHelper
                        (List.map Push range ++ remainingEffects)
                        ( context, wordType )

                Nothing ->
                    wordTypeFromStackEffectsHelper remainingEffects <|
                        ( context, { wordType | output = type_ :: wordType.output } )

        (Push type_) :: remainingEffects ->
            wordTypeFromStackEffectsHelper remainingEffects <|
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
                    ( Type.Union _, _ ) ->
                        -- Cannot go from union to concrete type
                        -- And we currently require unions to be exact matches
                        ( context, False )

                    ( _, Type.Union unionTypes ) ->
                        if List.member boundA unionTypes then
                            ( context, True )

                        else
                            case List.find isGeneric unionTypes of
                                Just generic ->
                                    compatibleTypes context typeA generic

                                Nothing ->
                                    ( context, False )

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


simplifyWordType : String -> ( Context, WordType ) -> ( Context, WordType )
simplifyWordType defName ( context, wordType ) =
    let
        oldSignature =
            wordType.input ++ wordType.output

        inputLength =
            List.length wordType.input

        aliases =
            oldSignature
                |> List.filterMap genericName
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
                                    String.fromChar nextId ++ "_" ++ defName
                            in
                            ( nextId
                                |> Char.toCode
                                |> (+) 1
                                |> Char.fromCode
                            , Dict.insert genName newName seenGenerics
                            , Type.Generic newName :: acc
                            )

                _ ->
                    ( nextId, seenGenerics, type_ :: acc )
    in
    ( context
    , { input = List.take inputLength newSignature
      , output = List.drop inputLength newSignature
      }
    )


genericName : Type -> Maybe String
genericName type_ =
    case type_ of
        Type.Generic name ->
            Just name

        _ ->
            Nothing


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
        Qualifier.Integer num ->
            IntLiteral num

        Qualifier.Word name ->
            case Dict.get name context.typedWords of
                Just def ->
                    Word def.name def.type_

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.WordRef ref ->
            WordRef ref

        Qualifier.ConstructType typeName ->
            case Dict.get typeName context.types of
                Just _ ->
                    ConstructType typeName

                Nothing ->
                    Debug.todo "Inconcievable!"

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

        Qualifier.Builtin builtin ->
            Builtin builtin


getMemberType : Dict String TypeDefinition -> String -> String -> Maybe Type
getMemberType typeDict typeName memberName =
    case Dict.get typeName typeDict of
        Just (CustomTypeDef _ members) ->
            List.find (\( name, _ ) -> name == memberName) members
                |> Maybe.map Tuple.second

        _ ->
            Nothing


typeDefName : TypeDefinition -> String
typeDefName typeDef =
    case typeDef of
        CustomTypeDef name _ ->
            name

        UnionTypeDef name _ ->
            name


isGeneric : Type -> Bool
isGeneric t =
    case t of
        Type.Generic _ ->
            True

        _ ->
            False
