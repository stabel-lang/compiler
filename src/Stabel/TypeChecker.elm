module Stabel.TypeChecker exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Set exposing (Set)
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.Metadata exposing (Metadata)
import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Stabel.Data.Type as Type exposing (FunctionType, Type)
import Stabel.Data.TypeSignature as TypeSignature exposing (TypeSignature)
import Stabel.Qualifier as Qualifier
import Stabel.TypeChecker.Problem exposing (Problem(..))


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type alias TypeDefinition =
    Qualifier.TypeDefinition


type alias WordDefinition =
    { name : String
    , type_ : FunctionType
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
    | Word SourceLocationRange String FunctionType
    | WordRef SourceLocationRange String
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type
    | Builtin SourceLocationRange Builtin


type alias Context =
    { types : Dict String TypeDefinition
    , typedWords : Dict String WordDefinition
    , untypedWords : Dict String Qualifier.FunctionDefinition
    , referenceableFunctions : Set String
    , stackEffects : List StackEffect
    , boundGenerics : Dict String Type
    , boundStackRanges : Dict String (List Type)
    , callStack : Set String
    , errors : List Problem
    }


type StackEffect
    = Push Type
    | Pop Type


type alias LoadedQualifierAST a =
    { a
        | types : Dict String Qualifier.TypeDefinition
        , functions : Dict String Qualifier.FunctionDefinition
        , referenceableFunctions : Set String
    }


initContext : LoadedQualifierAST a -> Context
initContext ast =
    let
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
            case t.members of
                Qualifier.StructMembers members ->
                    let
                        listedGenerics_ =
                            Set.fromList t.generics
                    in
                    members
                        |> List.map Tuple.second
                        |> collectReferencedGenerics
                        |> collectUndeclaredGenericProblems t.sourceLocation listedGenerics_

                Qualifier.UnionMembers mts ->
                    let
                        listedGenerics_ =
                            Set.fromList t.generics
                    in
                    mts
                        |> collectReferencedGenerics
                        |> collectUndeclaredGenericProblems t.sourceLocation listedGenerics_
    in
    { types = ast.types
    , typedWords = Dict.empty
    , untypedWords = ast.functions
    , referenceableFunctions = ast.referenceableFunctions
    , stackEffects = []
    , boundGenerics = Dict.empty
    , boundStackRanges = Dict.empty
    , callStack = Set.empty
    , errors = List.concatMap genericErrors (Dict.values ast.types)
    }


run : LoadedQualifierAST a -> Result (List Problem) AST
run ast =
    typeCheckHelper (initContext ast) ast


typeCheckHelper : Context -> LoadedQualifierAST a -> Result (List Problem) AST
typeCheckHelper context ast =
    let
        updatedContext =
            Dict.foldl (\_ v acc -> typeCheckDefinition v acc) context ast.functions
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = updatedContext.types
            , words = updatedContext.typedWords
            }

    else
        Err updatedContext.errors


typeCheckDefinition : Qualifier.FunctionDefinition -> Context -> Context
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


typeCheckSoloImplementation : Context -> Qualifier.FunctionDefinition -> List Qualifier.Node -> Context
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
                            untypedDef.typeSignature
                                |> TypeSignature.toMaybe
                                |> Maybe.withDefault inferredType
                        , metadata = metadataFromUntypedDef context untypedDef
                        , implementation = SoloImpl (untypedToTypedImplementation newContext impl)
                        }
                        newContext.typedWords
            }
    in
    verifyTypeSignature inferredType untypedDef finalContext
        |> cleanContext


untypedToTypedImplementation : Context -> List Qualifier.Node -> List AstNode
untypedToTypedImplementation context impl =
    let
        helper node ( idx, res ) =
            ( idx + 1
            , untypedToTypedNode idx context node :: res
            )
    in
    List.foldl helper ( 0, [] ) impl
        |> Tuple.second
        |> List.reverse


typeCheckMultiImplementation :
    Context
    -> Qualifier.FunctionDefinition
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
            whens
                |> List.foldr (inferWhenTypes untypedDef) ( [], context )
                |> Tuple.mapFirst normalizeWhenTypes
                |> (\( wts, ctx ) -> simplifyWhenWordTypes wts ctx)
                |> Tuple.mapFirst (List.map2 Tuple.pair whenPatterns >> List.map replaceFirstTypeWithPatternMatch)
                |> Tuple.mapFirst equalizeWhenTypes
                |> Tuple.mapFirst (\whenTypes -> List.map (constrainGenerics untypedDef.typeSignature) whenTypes)

        replaceFirstTypeWithPatternMatch ( Qualifier.TypeMatch _ matchType _, typeSignature ) =
            case typeSignature.input of
                ((Type.Generic _) as toReplace) :: _ ->
                    { input = List.map (replaceType toReplace matchType) typeSignature.input
                    , output = List.map (replaceType toReplace matchType) typeSignature.output
                    }

                ((Type.StackRange _) as toReplace) :: _ ->
                    { input = List.map (replaceType toReplace matchType) typeSignature.input
                    , output = List.map (replaceType toReplace matchType) typeSignature.output
                    }

                _ ->
                    typeSignature

        replaceType type_ with el =
            case el of
                Type.CustomGeneric name members ->
                    Type.CustomGeneric name (List.map (replaceType type_ with) members)

                Type.Union name members ->
                    Type.Union name (List.map (replaceType type_ with) members)

                Type.FunctionSignature quotType ->
                    Type.FunctionSignature
                        { input = List.map (replaceType type_ with) quotType.input
                        , output = List.map (replaceType type_ with) quotType.output
                        }

                _ ->
                    if type_ == el then
                        with

                    else
                        el

        whenPatterns =
            List.map Tuple.first whens

        whensAreConsistent =
            inferredWhenTypes
                |> List.map2 Tuple.pair whenPatterns
                |> List.all typeCheckWhen

        typeCheckWhen ( Qualifier.TypeMatch _ forType _, inf ) =
            case inf.input of
                firstInput :: _ ->
                    Type.genericlyCompatible firstInput forType

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
            List.head inferredWhenTypes
                |> Maybe.withDefault { input = [], output = [] }
                |> replaceFirstType (unionOfTypeMatches whens)
                |> joinOutputs (List.map .output inferredWhenTypes)

        exposedType =
            TypeSignature.toMaybe untypedDef.typeSignature
                |> Maybe.withDefault inferredType

        maybeConsistencyError =
            if whensAreConsistent && whensAreCompatible then
                Nothing

            else
                let
                    error =
                        InconsistentWhens
                            (Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation)
                            untypedDef.name
                in
                Just error

        maybeInexhaustiveError =
            inexhaustivenessCheck
                (Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation)
                whenPatterns

        finalContext =
            { newContext
                | typedWords =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , type_ = exposedType
                        , metadata = metadataFromUntypedDef context untypedDef
                        , implementation =
                            MultiImpl
                                (List.map (Tuple.mapBoth mapTypeMatch (untypedToTypedImplementation newContext)) initialWhens)
                                (untypedToTypedImplementation newContext defaultImpl)
                        }
                        newContext.typedWords
                , errors =
                    List.filterMap identity
                        [ maybeConsistencyError
                        , maybeInexhaustiveError
                        ]
                        ++ newContext.errors
            }
    in
    verifyTypeSignature inferredType untypedDef finalContext
        |> cleanContext


metadataFromUntypedDef : Context -> Qualifier.FunctionDefinition -> Metadata
metadataFromUntypedDef context def =
    { isEntryPoint = False
    , type_ = def.typeSignature
    , isInline = Set.member def.name context.referenceableFunctions
    , sourceLocationRange = def.sourceLocation
    , isExposed = def.exposed
    }


inferWhenTypes :
    Qualifier.FunctionDefinition
    -> ( Qualifier.TypeMatch, List Qualifier.Node )
    -> ( List FunctionType, Context )
    -> ( List FunctionType, Context )
inferWhenTypes untypedDef ( Qualifier.TypeMatch _ t _, im ) ( infs, ctx ) =
    let
        alteredTypeSignature =
            case untypedDef.typeSignature of
                TypeSignature.UserProvided wt ->
                    TypeSignature.UserProvided <|
                        case wt.input of
                            _ :: rest ->
                                { wt | input = t :: rest }

                            _ ->
                                wt

                x ->
                    x

        alteredDef =
            { untypedDef | typeSignature = alteredTypeSignature }

        ( inf, newCtx ) =
            typeCheckImplementation alteredDef im (cleanContext ctx)
    in
    ( inf :: infs, newCtx )


normalizeWhenTypes : List FunctionType -> List FunctionType
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


simplifyWhenWordTypes : List FunctionType -> Context -> ( List FunctionType, Context )
simplifyWhenWordTypes wordTypes context =
    ( List.map (\wt -> Tuple.second (simplifyWordType ( context, wt ))) wordTypes
    , context
    )


equalizeWhenTypes : List FunctionType -> List FunctionType
equalizeWhenTypes wordTypes =
    let
        splitFirstInputType wordType =
            case wordType.input of
                first :: rest ->
                    Just ( first, { wordType | input = rest } )

                _ ->
                    -- Should never happen
                    Nothing

        joinSplitWordType ( firstType, wordType ) =
            { wordType | input = firstType :: wordType.input }
    in
    List.filterMap splitFirstInputType wordTypes
        |> List.foldr
            (\( firstType, wordType ) ( typeAcc, wordTypeAcc ) -> ( firstType :: typeAcc, wordType :: wordTypeAcc ))
            ( [], [] )
        |> Tuple.mapSecond (\lobotomizedWordTypes -> equalizeWhenTypesHelper lobotomizedWordTypes Dict.empty [])
        |> (\( firstTypes, equalizedWhenTypes ) -> List.map2 Tuple.pair firstTypes equalizedWhenTypes)
        |> List.map joinSplitWordType


equalizeWhenTypesHelper : List FunctionType -> Dict String Type -> List FunctionType -> List FunctionType
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

                ( unzippedFirstInputs, unzippedSecondInputs ) =
                    List.foldr unzip ( [], [] ) constrainedInputs

                newFirstType =
                    { input = unzippedFirstInputs
                    , output = firstType.output
                    }

                newSecondType =
                    { input = unzippedSecondInputs
                    , output = secondType.output
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
                |> List.concatMap flattenUnions
                |> List.gatherEquals
                |> List.map Tuple.first

        flattenUnions t =
            case t of
                Type.Union _ members ->
                    List.concatMap flattenUnions members

                _ ->
                    [ t ]
    in
    case uniqueTypes of
        [ singleType ] ->
            singleType

        _ ->
            Type.Union Nothing uniqueTypes


replaceFirstType : Type -> FunctionType -> FunctionType
replaceFirstType with inf =
    case inf.input of
        _ :: rem ->
            { inf | input = with :: rem }

        _ ->
            inf


joinOutputs : List (List Type) -> FunctionType -> FunctionType
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
                                Type.Union Nothing [ lhs, rhs ]
            in
            joinOutputs (joined :: rest) result

        joined :: [] ->
            { result | output = joined }

        _ ->
            result


constrainGenerics : TypeSignature -> FunctionType -> FunctionType
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


constrainGenericsHelper : Dict String Type -> List Type -> List Type -> List Type -> ( Dict String Type, List Type )
constrainGenericsHelper remappedGenerics annotated inferred acc =
    case ( annotated, inferred ) of
        ( [], _ ) ->
            ( remappedGenerics, List.reverse acc )

        ( _, [] ) ->
            ( remappedGenerics, List.reverse acc )

        ( ((Type.Generic annGen) as annotatedEl) :: annotatedRest, ((Type.Generic infGen) as inferredEl) :: inferredRest ) ->
            if annGen == infGen then
                constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)

            else
                case Dict.get infGen remappedGenerics of
                    Just val ->
                        constrainGenericsHelper remappedGenerics annotatedRest inferredRest (val :: acc)

                    Nothing ->
                        constrainGenericsHelper
                            (Dict.insert infGen annotatedEl remappedGenerics)
                            annotatedRest
                            inferredRest
                            (annotatedEl :: acc)

        ( ((Type.StackRange annGen) as annotatedEl) :: annotatedRest, ((Type.StackRange infGen) as inferredEl) :: inferredRest ) ->
            if annGen == infGen then
                constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)

            else
                case Dict.get infGen remappedGenerics of
                    Just val ->
                        constrainGenericsHelper remappedGenerics annotatedRest inferredRest (val :: acc)

                    Nothing ->
                        constrainGenericsHelper
                            (Dict.insert infGen annotatedEl remappedGenerics)
                            annotatedRest
                            inferredRest
                            (annotatedEl :: acc)

        ( (_ as annotatedEl) :: annotatedRest, (Type.StackRange infGen) :: inferredRest ) ->
            case Dict.get infGen remappedGenerics of
                Just val ->
                    constrainGenericsHelper remappedGenerics annotatedRest inferredRest (val :: acc)

                Nothing ->
                    constrainGenericsHelper
                        (Dict.insert infGen annotatedEl remappedGenerics)
                        annotatedRest
                        inferredRest
                        (annotatedEl :: acc)

        ( (Type.FunctionSignature annotatedQuote) :: annotatedRest, (Type.FunctionSignature inferredQuote) :: inferredRest ) ->
            let
                ( quoteRemappedGens, constrainedInputs ) =
                    constrainGenericsHelper remappedGenerics annotatedQuote.input inferredQuote.input []

                ( quoteRemappedGens2, constrainedOutputs ) =
                    constrainGenericsHelper quoteRemappedGens annotatedQuote.output inferredQuote.output []

                constrainedQuote =
                    Type.FunctionSignature
                        { input = constrainedInputs
                        , output = constrainedOutputs
                        }
            in
            constrainGenericsHelper
                (Dict.union quoteRemappedGens2 remappedGenerics)
                annotatedRest
                inferredRest
                (constrainedQuote :: acc)

        ( _ :: annotatedRest, inferredEl :: inferredRest ) ->
            constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)


typeCheckImplementation : Qualifier.FunctionDefinition -> List Qualifier.Node -> Context -> ( FunctionType, Context )
typeCheckImplementation untypedDef impl context =
    let
        startingStackEffects =
            untypedDef.typeSignature
                |> TypeSignature.toMaybe
                |> Maybe.map reverseWordType
                |> Maybe.withDefault Type.emptyFunctionType
                |> wordTypeToStackEffects

        reverseWordType wt =
            { input = []
            , output = wt.input
            }

        contextWithCall =
            { context
                | callStack = Set.insert untypedDef.name context.callStack
                , stackEffects = startingStackEffects
            }

        ( _, contextWithStackEffects ) =
            List.foldl
                (\node ( idx, ctx ) -> ( idx + 1, typeCheckNode idx node ctx ))
                ( 0, contextWithCall )
                impl

        contextWithoutCall =
            { contextWithStackEffects | callStack = Set.remove untypedDef.name contextWithStackEffects.callStack }

        annotatedInput =
            untypedDef.typeSignature
                |> TypeSignature.toMaybe
                |> Maybe.map .input
                |> Maybe.withDefault []
    in
    wordTypeFromStackEffects untypedDef contextWithoutCall
        |> (\( ctx, wt ) -> ( ctx, { wt | input = wt.input ++ annotatedInput } ))
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


inexhaustivenessCheck : SourceLocationRange -> List Qualifier.TypeMatch -> Maybe Problem
inexhaustivenessCheck range patterns =
    let
        inexhaustiveStates =
            List.foldl (inexhaustivenessCheckHelper []) [] patterns
                |> List.filter (\( _, state ) -> state /= Total)
                |> List.map Tuple.first
    in
    case inexhaustiveStates of
        [] ->
            Nothing

        _ ->
            Just (InexhaustiveMultiWord range inexhaustiveStates)


type InexhaustiveState
    = Total
    | SeenInt
    | SeenType Type


inexhaustivenessCheckHelper : List Type -> Qualifier.TypeMatch -> List ( List Type, InexhaustiveState ) -> List ( List Type, InexhaustiveState )
inexhaustivenessCheckHelper typePrefix (Qualifier.TypeMatch _ t conds) acc =
    let
        typeList =
            typePrefix ++ [ t ]
    in
    if List.any (\( toMatch, state ) -> typeList == toMatch && state == Total) acc then
        acc

    else
        let
            subcases =
                conds
                    |> List.map Tuple.second
                    |> List.filterMap isRecursiveMatch
                    |> List.foldl (inexhaustivenessCheckHelper typeList) acc

            isRecursiveMatch match =
                case match of
                    Qualifier.RecursiveMatch cond ->
                        Just cond

                    _ ->
                        Nothing

            toAdd =
                case ( t, conds, subcases ) of
                    ( _, [], _ ) ->
                        [ ( typeList, Total ) ]

                    ( Type.Int, _, _ ) ->
                        [ ( typeList, SeenInt ) ]

                    _ ->
                        if List.all (Tuple.second >> (==) Total) subcases then
                            [ ( typeList, Total ) ]

                        else
                            subcases

            modifiedAcc =
                if toAdd /= [ ( typeList, Total ) ] then
                    acc

                else
                    List.filter
                        (\( toMatch, _ ) ->
                            List.take (List.length typeList) toMatch /= typeList
                        )
                        acc
        in
        if List.find (\( toMatch, _ ) -> toMatch == typeList) modifiedAcc == Nothing then
            toAdd ++ modifiedAcc

        else
            let
                updatedStates =
                    List.filter (\( toMatch, _ ) -> toMatch /= typeList) modifiedAcc
            in
            toAdd ++ updatedStates


verifyTypeSignature : FunctionType -> Qualifier.FunctionDefinition -> Context -> Context
verifyTypeSignature inferredType untypedDef context =
    case TypeSignature.toMaybe untypedDef.typeSignature of
        Just annotatedType ->
            let
                ( _, simplifiedAnnotatedType ) =
                    simplifyWordType ( context, annotatedType )
            in
            if not <| Type.compatibleFunctions simplifiedAnnotatedType inferredType then
                let
                    range =
                        untypedDef.sourceLocation
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

        Qualifier.Function _ name ->
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
                                case TypeSignature.toMaybe untypedDef.typeSignature of
                                    Just annotatedType ->
                                        addStackEffect context <| wordTypeToStackEffects annotatedType

                                    Nothing ->
                                        let
                                            problem =
                                                MissingTypeAnnotationInRecursiveCallStack
                                                    (Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation)
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

        Qualifier.FunctionRef loc ref ->
            let
                stackEffectsBeforeWordCheck =
                    context.stackEffects

                contextAfterWordCheck =
                    typeCheckNode idx (Qualifier.Function loc ref) context

                newContext =
                    { contextAfterWordCheck | stackEffects = stackEffectsBeforeWordCheck }
            in
            case Dict.get ref newContext.typedWords of
                Just def ->
                    addStackEffect newContext <|
                        [ Push <| Type.FunctionSignature def.type_ ]

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.ConstructType typeName ->
            case getMembers typeName context.types of
                Just (Qualifier.StructMembers members) ->
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
                ( getMembers typeName context.types
                , getMemberType context.types typeName memberName
                )
            of
                ( Just (Qualifier.StructMembers members), Just memberType ) ->
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
                ( getMembers typeName context.types
                , getMemberType context.types typeName memberName
                )
            of
                ( Just (Qualifier.StructMembers members), Just memberType ) ->
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


getMembers : String -> Dict String TypeDefinition -> Maybe Qualifier.TypeDefinitionMembers
getMembers typeName types =
    Dict.get typeName types
        |> Maybe.map .members


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

        Type.Union name members ->
            Type.Union name (List.map (tagGeneric idx) members)

        Type.FunctionSignature wt ->
            Type.FunctionSignature
                { input = List.map (tagGeneric idx) wt.input
                , output = List.map (tagGeneric idx) wt.output
                }

        _ ->
            type_


wordTypeToStackEffects : FunctionType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Pop (List.reverse wordType.input)
        ++ List.map Push wordType.output


wordTypeFromStackEffects : Qualifier.FunctionDefinition -> Context -> ( Context, FunctionType )
wordTypeFromStackEffects untypedDef context =
    wordTypeFromStackEffectsHelper untypedDef context.stackEffects ( context, { input = [], output = [] } )


wordTypeFromStackEffectsHelper : Qualifier.FunctionDefinition -> List StackEffect -> ( Context, FunctionType ) -> ( Context, FunctionType )
wordTypeFromStackEffectsHelper untypedDef effects ( context, wordType ) =
    let
        problem expected actual =
            UnexpectedType
                (Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation)
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
                    ( Type.Union _ leftUnion, Type.Union _ rightUnion ) ->
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

                    ( Type.Union _ _, _ ) ->
                        -- Cannot go from union to concrete type
                        ( context, False )

                    ( lhsType, Type.Union _ unionTypes ) ->
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

                    ( Type.FunctionSignature lhs, Type.FunctionSignature rhs ) ->
                        let
                            boundRanges =
                                Dict.empty
                                    |> bindStackRange context lhs.input rhs.input
                                    |> bindStackRange context lhs.output rhs.output

                            actualInputRequirement =
                                replaceStackRange boundRanges rhs.input

                            actualOutputRequirement =
                                replaceStackRange boundRanges rhs.output

                            contextWithBoundRanges =
                                { context | boundStackRanges = Dict.union context.boundStackRanges boundRanges }

                            ( contextAfterInputCheck, inputsCompatible ) =
                                List.map2 Tuple.pair lhs.input actualInputRequirement
                                    |> List.foldl foldHelper ( contextWithBoundRanges, True )

                            ( contextAfterOutputCheck, outputsCompatible ) =
                                List.map2 Tuple.pair lhs.output actualOutputRequirement
                                    |> List.foldl foldHelper ( contextAfterInputCheck, True )

                            foldHelper ( lType, rType ) (( currCtx, isCompatible ) as acc) =
                                if not isCompatible then
                                    acc

                                else
                                    compatibleTypes currCtx lType rType
                        in
                        ( contextAfterOutputCheck
                        , inputsCompatible && outputsCompatible
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


simplifyWordType : ( Context, FunctionType ) -> ( Context, FunctionType )
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

                Type.Union name members ->
                    Type.Union name (List.map reduceGenericName members)

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

                Type.Union name members ->
                    let
                        ( newNextId, newSeenGenerics, newMembers ) =
                            List.foldr renameGenerics ( nextId, seenGenerics, [] ) members
                    in
                    ( newNextId, newSeenGenerics, Type.Union name newMembers :: acc )

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


untypedToTypedNode : Int -> Context -> Qualifier.Node -> AstNode
untypedToTypedNode idx context untypedNode =
    case untypedNode of
        Qualifier.Integer range num ->
            IntLiteral range num

        Qualifier.Function range name ->
            case Dict.get name context.typedWords of
                Just def ->
                    let
                        resolvedWordType =
                            { input =
                                def.type_.input
                                    |> List.map (tagGeneric idx >> replaceGenericWithBoundValue)
                            , output =
                                def.type_.output
                                    |> List.map (tagGeneric idx >> replaceGenericWithBoundValue)
                            }

                        replaceGenericWithBoundValue t =
                            let
                                boundType =
                                    case getGenericBinding context t of
                                        Just boundValue ->
                                            boundValue

                                        Nothing ->
                                            t
                            in
                            case boundType of
                                Type.Union unionName members ->
                                    Type.Union unionName <|
                                        List.map replaceGenericWithBoundValue members

                                _ ->
                                    boundType
                    in
                    Word range name resolvedWordType

                Nothing ->
                    Dict.get name context.untypedWords
                        |> Maybe.andThen (.typeSignature >> TypeSignature.toMaybe)
                        |> Maybe.withDefault { input = [], output = [] }
                        |> Word range name

        Qualifier.FunctionRef range ref ->
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
    case getMembers typeName typeDict of
        Just (Qualifier.StructMembers members) ->
            List.find (\( name, _ ) -> name == memberName) members
                |> Maybe.map Tuple.second

        _ ->
            Nothing
