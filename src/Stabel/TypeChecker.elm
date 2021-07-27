module Stabel.TypeChecker exposing
    ( AST
    , AstNode(..)
    , CycleData
    , FunctionDefinition
    , FunctionImplementation(..)
    , TypeDefinition
    , TypeMatch(..)
    , TypeMatchValue(..)
    , run
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Set exposing (Set)
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Stabel.Data.Type as Type exposing (FunctionType, Type)
import Stabel.Data.TypeSignature as TypeSignature exposing (TypeSignature)
import Stabel.Qualifier as Qualifier
import Stabel.TypeChecker.Problem exposing (Problem(..))


type alias AST =
    { types : Dict String TypeDefinition
    , functions : Dict String FunctionDefinition
    }


type alias TypeDefinition =
    Qualifier.TypeDefinition


type alias FunctionDefinition =
    { name : String
    , sourceLocation : Maybe SourceLocationRange
    , type_ : FunctionType
    , isInline : Bool
    , implementation : FunctionImplementation
    }


type FunctionImplementation
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
    | Function SourceLocationRange String FunctionType
    | FunctionRef SourceLocationRange String
    | Recurse SourceLocationRange
    | Cycle SourceLocationRange CycleData
    | Builtin SourceLocationRange Builtin
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type


type alias CycleData =
    { name : String
    , sourceLocation : Maybe SourceLocationRange
    , typeSignature : FunctionType
    }


type StackEffect
    = Push Type
    | Pop Type


run : Qualifier.AST -> Result (List Problem) AST
run ast =
    typeCheck (initContext ast) ast



-- CONTEXT --


type alias Context =
    { types : Dict String TypeDefinition
    , typedFunctions : Dict String FunctionDefinition
    , untypedFunctions : Dict String Qualifier.FunctionDefinition
    , referenceableFunctions : Set String
    , stackEffects : List StackEffect
    , boundGenerics : Dict String Type
    , boundStackRanges : Dict String (List Type)
    , errors : List Problem
    }


initContext : Qualifier.AST -> Context
initContext ast =
    { types = ast.types
    , typedFunctions = Dict.empty
    , untypedFunctions = ast.functions
    , referenceableFunctions = ast.referenceableFunctions
    , stackEffects = []
    , boundGenerics = Dict.empty
    , boundStackRanges = Dict.empty
    , errors =
        ast.types
            |> Dict.values
            |> List.concatMap verifyGenericVariableConsistency
    }


verifyGenericVariableConsistency : Qualifier.TypeDefinition -> List Problem
verifyGenericVariableConsistency t =
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


collectReferencedGenerics : List Type -> List String
collectReferencedGenerics memberTypes =
    List.map Type.referencedGenerics memberTypes
        |> List.foldl Set.union Set.empty
        |> Set.toList


collectUndeclaredGenericProblems : SourceLocationRange -> Set String -> List String -> List Problem
collectUndeclaredGenericProblems range listedGenerics memberTypes =
    memberTypes
        |> List.filter (\gen -> not (Set.member gen listedGenerics))
        |> List.map (\gen -> UndeclaredGeneric range gen listedGenerics)


cleanContext : Context -> Context
cleanContext ctx =
    { ctx
        | stackEffects = []
        , boundGenerics = Dict.empty
        , boundStackRanges = Dict.empty
    }



-- Type Checking --


typeCheck : Context -> Qualifier.AST -> Result (List Problem) AST
typeCheck context ast =
    let
        updatedContext =
            Dict.foldl (\_ v acc -> typeCheckDefinition v acc) context ast.functions
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = updatedContext.types
            , functions = updatedContext.typedFunctions
            }

    else
        Err updatedContext.errors


typeCheckDefinition : Qualifier.FunctionDefinition -> Context -> Context
typeCheckDefinition untypedDef context =
    case Dict.get untypedDef.name context.typedFunctions of
        Just _ ->
            context

        Nothing ->
            case untypedDef.implementation of
                Qualifier.SoloImpl impl ->
                    typeCheckSoloImplementation context untypedDef impl

                Qualifier.MultiImpl initialWhens defaultImpl ->
                    typeCheckMultiImplementation context untypedDef initialWhens defaultImpl



-- Type Check Solo Impl --


typeCheckSoloImplementation : Context -> Qualifier.FunctionDefinition -> List Qualifier.Node -> Context
typeCheckSoloImplementation context untypedDef impl =
    let
        ( inferredType, newContext ) =
            typeCheckImplementation
                untypedDef
                untypedDef.typeSignature
                impl
                (cleanContext context)

        typedImplementation =
            SoloImpl (untypedToTypedImplementation newContext impl)

        finalContext =
            { newContext
                | typedFunctions =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , sourceLocation = untypedDef.sourceLocation
                        , type_ =
                            untypedDef.typeSignature
                                |> TypeSignature.withDefault inferredType
                        , isInline = Set.member untypedDef.name context.referenceableFunctions
                        , implementation = typedImplementation
                        }
                        newContext.typedFunctions
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


untypedToTypedNode : Int -> Context -> Qualifier.Node -> AstNode
untypedToTypedNode idx context untypedNode =
    case untypedNode of
        Qualifier.Integer range num ->
            IntLiteral range num

        Qualifier.Function range function ->
            case Dict.get function.name context.typedFunctions of
                Just def ->
                    let
                        resolvedFunctionType =
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
                                -- TODO: not structs?
                                Type.Union unionName members ->
                                    Type.Union unionName <|
                                        List.map replaceGenericWithBoundValue members

                                _ ->
                                    boundType
                    in
                    Function range function.name resolvedFunctionType

                Nothing ->
                    -- TODO: this can't be right?
                    Dict.get function.name context.untypedFunctions
                        |> Maybe.andThen (.typeSignature >> TypeSignature.toMaybe)
                        |> Maybe.withDefault { input = [], output = [] }
                        |> Function range function.name

        Qualifier.FunctionRef range ref ->
            FunctionRef range ref.name

        Qualifier.Recurse range ->
            Recurse range

        Qualifier.Cycle range data ->
            -- TODO: Not right
            Cycle range
                { name = data.name
                , sourceLocation = data.sourceLocation
                , typeSignature = { input = [], output = [] }
                }

        Qualifier.Builtin range builtin ->
            Builtin range builtin

        Qualifier.ConstructType typeDef ->
            ConstructType typeDef.name

        Qualifier.SetMember typeDef memberName memberType ->
            SetMember typeDef.name memberName memberType

        Qualifier.GetMember typeDef memberName memberType ->
            GetMember typeDef.name memberName memberType



-- Type Check Multi functions --


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
                            typeCheckImplementation
                                untypedDef
                                untypedDef.typeSignature
                                defaultImpl
                                (cleanContext context)
                    in
                    case inferredDefaultType.input of
                        [] ->
                            -- TODO: Generic arg?
                            -- TODO: Add test to see if we can make this fail
                            Debug.todo "Default impl doesn't have an input argument"

                        firstType :: _ ->
                            ( Qualifier.TypeMatch SourceLocation.emptyRange firstType [], defaultImpl ) :: initialWhens

        ( inferredWhenTypes, newContext ) =
            whens
                |> List.foldr (inferWhenTypes untypedDef) ( [], context )
                |> Tuple.mapFirst normalizeWhenTypes
                |> (\( wts, ctx ) -> simplifyWhenFunctionTypes wts ctx)
                |> Tuple.mapFirst (List.map2 Tuple.pair whenPatterns >> List.map replaceFirstTypeWithPatternMatch)
                |> Tuple.mapFirst equalizeWhenTypes
                |> Tuple.mapFirst (List.map (constrainGenerics untypedDef.typeSignature))

        whenPatterns =
            List.map Tuple.first whens

        whensAreConsistent =
            inferredWhenTypes
                |> List.map2 Tuple.pair whenPatterns
                |> List.all patternMatchIsCompatibleWithInferredType

        whensAreCompatible =
            inferredWhenTypes
                |> List.map (dropFirstInputType >> countOutput)
                |> areAllEqual

        inferredType =
            List.head inferredWhenTypes
                |> Maybe.withDefault { input = [], output = [] }
                |> replaceFirstInputType (unionOfTypeMatches whens)
                |> joinOutputs (List.map .output inferredWhenTypes)

        exposedType =
            TypeSignature.withDefault inferredType untypedDef.typeSignature

        maybeConsistencyError =
            if whensAreConsistent && whensAreCompatible then
                Nothing

            else
                Just <| InconsistentWhens sourceLocation untypedDef.name

        maybeInexhaustiveError =
            inexhaustivenessCheck sourceLocation whenPatterns

        sourceLocation =
            Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation

        finalContext =
            { newContext
                | typedFunctions =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , sourceLocation = untypedDef.sourceLocation
                        , type_ = exposedType
                        , isInline = Set.member untypedDef.name context.referenceableFunctions
                        , implementation =
                            MultiImpl
                                (List.map
                                    (Tuple.mapBoth mapTypeMatch typeImplementation)
                                    initialWhens
                                )
                                (typeImplementation defaultImpl)
                        }
                        newContext.typedFunctions
                , errors =
                    List.filterMap identity
                        [ maybeConsistencyError
                        , maybeInexhaustiveError
                        ]
                        ++ newContext.errors
            }

        typeImplementation =
            untypedToTypedImplementation newContext
    in
    verifyTypeSignature inferredType untypedDef finalContext
        |> cleanContext


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

        ( inf, newCtx ) =
            typeCheckImplementation untypedDef alteredTypeSignature im (cleanContext ctx)
    in
    ( inf :: infs, newCtx )


normalizeWhenTypes : List FunctionType -> List FunctionType
normalizeWhenTypes whenTypes =
    let
        maybeLongestInputWhenType =
            List.sortBy (.input >> List.length) whenTypes
                |> List.reverse
                |> List.head

        matchInputLength toMatch functionType =
            let
                diff =
                    List.length toMatch.input - List.length functionType.input

                padding =
                    List.take diff toMatch.input
                        |> List.map padGeneric
            in
            case padding of
                [] ->
                    functionType

                elements ->
                    { input = elements ++ functionType.input
                    , output = elements ++ functionType.output
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


simplifyWhenFunctionTypes : List FunctionType -> Context -> ( List FunctionType, Context )
simplifyWhenFunctionTypes functionTypes context =
    ( List.map (\wt -> Tuple.first (simplifyFunctionType ( wt, context ))) functionTypes
    , context
    )


replaceFirstTypeWithPatternMatch : ( Qualifier.TypeMatch, FunctionType ) -> FunctionType
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


replaceType : Type -> Type -> Type -> Type
replaceType type_ with el =
    case el of
        Type.CustomGeneric name members ->
            Type.CustomGeneric name (List.map (replaceType type_ with) members)

        Type.Union name members ->
            Type.Union name (List.map (replaceType type_ with) members)

        Type.FunctionSignature functionType ->
            Type.FunctionSignature
                { input = List.map (replaceType type_ with) functionType.input
                , output = List.map (replaceType type_ with) functionType.output
                }

        _ ->
            if type_ == el then
                with

            else
                el


equalizeWhenTypes : List FunctionType -> List FunctionType
equalizeWhenTypes functionTypes =
    let
        splitFirstInputType functionType =
            case functionType.input of
                first :: rest ->
                    Just ( first, { functionType | input = rest } )

                _ ->
                    -- Should never happen
                    Nothing

        joinSplitFunctionType ( firstType, functionType ) =
            { functionType | input = firstType :: functionType.input }
    in
    List.filterMap splitFirstInputType functionTypes
        |> List.foldr
            (\( firstType, functionType ) ( typeAcc, functionTypeAcc ) -> ( firstType :: typeAcc, functionType :: functionTypeAcc ))
            ( [], [] )
        |> Tuple.mapSecond (\lobotomizedFunctionTypes -> equalizeWhenTypesHelper lobotomizedFunctionTypes Dict.empty [])
        |> (\( firstTypes, equalizedWhenTypes ) -> List.map2 Tuple.pair firstTypes equalizedWhenTypes)
        |> List.map joinSplitFunctionType


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


replaceFirstInputType : Type -> FunctionType -> FunctionType
replaceFirstInputType with inf =
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
                        ( Type.Union _ lhsMems, Type.Union _ rhsMems ) ->
                            if lhsMems == rhsMems then
                                lhs

                            else
                                Type.Union Nothing (lhsMems ++ rhsMems)

                        ( Type.Union _ lhsMems, _ ) ->
                            if List.member rhs lhsMems then
                                lhs

                            else
                                Type.Union Nothing (rhs :: lhsMems)

                        ( _, Type.Union _ rhsMems ) ->
                            if List.member lhs rhsMems then
                                rhs

                            else
                                Type.Union Nothing (lhs :: rhsMems)

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
        ( [], rest ) ->
            ( remappedGenerics, List.reverse acc ++ rest )

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

        ( (Type.FunctionSignature annotatedFunction) :: annotatedRest, (Type.FunctionSignature inferredFunction) :: inferredRest ) ->
            let
                ( functionRemappedGens, constrainedInputs ) =
                    constrainGenericsHelper remappedGenerics annotatedFunction.input inferredFunction.input []

                ( functionRemappedGens2, constrainedOutputs ) =
                    constrainGenericsHelper functionRemappedGens annotatedFunction.output inferredFunction.output []

                constrainedFunction =
                    Type.FunctionSignature
                        { input = constrainedInputs
                        , output = constrainedOutputs
                        }
            in
            constrainGenericsHelper
                (Dict.union functionRemappedGens2 remappedGenerics)
                annotatedRest
                inferredRest
                (constrainedFunction :: acc)

        ( _ :: annotatedRest, inferredEl :: inferredRest ) ->
            constrainGenericsHelper remappedGenerics annotatedRest inferredRest (inferredEl :: acc)


patternMatchIsCompatibleWithInferredType : ( Qualifier.TypeMatch, FunctionType ) -> Bool
patternMatchIsCompatibleWithInferredType ( Qualifier.TypeMatch _ forType _, inf ) =
    case inf.input of
        firstInput :: _ ->
            Type.genericlyCompatible firstInput forType

        [] ->
            False


dropFirstInputType : FunctionType -> FunctionType
dropFirstInputType inf =
    { inf | input = List.drop 1 inf.input }


countOutput : FunctionType -> ( List Type, Int )
countOutput functionType =
    ( functionType.input, List.length functionType.output )


areAllEqual : List ( List Type, Int ) -> Bool
areAllEqual ls =
    case ls of
        [] ->
            True

        ( inputTypes, outputLength ) :: rest ->
            List.all
                (\( nextInputTypes, nextOutputLength ) ->
                    (outputLength == nextOutputLength)
                        && compatibleTypeList inputTypes nextInputTypes
                )
                rest


compatibleTypeList : List Type -> List Type -> Bool
compatibleTypeList aLs bLs =
    List.map2 Type.genericlyCompatible aLs bLs
        |> List.all identity


typeCheckImplementation :
    Qualifier.FunctionDefinition
    -> TypeSignature
    -> List Qualifier.Node
    -> Context
    -> ( FunctionType, Context )
typeCheckImplementation untypedDef typeSignatureToUse impl context =
    let
        startingStackEffects =
            typeSignatureToUse
                |> TypeSignature.map reverseFunctionType
                |> TypeSignature.withDefault Type.emptyFunctionType
                |> functionTypeToStackEffects

        reverseFunctionType wt =
            { input = []
            , output = wt.input
            }

        contextWithCall =
            { context | stackEffects = startingStackEffects }

        ( _, contextWithStackEffects ) =
            List.foldl
                (\node ( idx, ctx ) -> ( idx + 1, typeCheckNode untypedDef idx node ctx ))
                ( 0, contextWithCall )
                impl

        annotatedInput =
            typeSignatureToUse
                |> TypeSignature.toMaybe
                |> Maybe.map .input
                |> Maybe.withDefault []
    in
    functionTypeFromStackEffects untypedDef contextWithStackEffects
        |> (\( ctx, wt ) -> ( { wt | input = wt.input ++ annotatedInput }, ctx ))
        |> simplifyFunctionType


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
            Just (InexhaustiveMultiFunction range inexhaustiveStates)


type InexhaustiveState
    = Total
    | SeenInt


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
        if List.any (\( toMatch, _ ) -> toMatch == typeList) modifiedAcc then
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
                simplifiedAnnotatedType =
                    Tuple.first <| simplifyFunctionType ( annotatedType, context )
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


typeCheckNode : Qualifier.FunctionDefinition -> Int -> Qualifier.Node -> Context -> Context
typeCheckNode currentDef idx node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = ctx.stackEffects ++ List.map (tagGenericEffect idx) effects }
    in
    case node of
        Qualifier.Integer _ _ ->
            addStackEffect context [ Push Type.Int ]

        Qualifier.Function _ untypedDef ->
            case Dict.get untypedDef.name context.typedFunctions of
                Just def ->
                    addStackEffect context <| functionTypeToStackEffects def.type_

                Nothing ->
                    let
                        contextWithTypedDef =
                            typeCheckDefinition untypedDef context

                        newContext =
                            { contextWithTypedDef | stackEffects = context.stackEffects }
                    in
                    case Dict.get untypedDef.name newContext.typedFunctions of
                        Nothing ->
                            Debug.todo "inconcievable!"

                        Just def ->
                            addStackEffect newContext <| functionTypeToStackEffects def.type_

        Qualifier.FunctionRef loc ref ->
            let
                stackEffectsBeforeFunctionCheck =
                    context.stackEffects

                contextAfterFunctionCheck =
                    typeCheckNode currentDef idx (Qualifier.Function loc ref) context

                newContext =
                    { contextAfterFunctionCheck | stackEffects = stackEffectsBeforeFunctionCheck }
            in
            case Dict.get ref.name newContext.typedFunctions of
                Just def ->
                    addStackEffect newContext <|
                        [ Push <| Type.FunctionSignature def.type_ ]

                _ ->
                    Debug.todo "inconcievable!"

        Qualifier.Recurse _ ->
            case TypeSignature.toMaybe currentDef.typeSignature of
                Just annotatedType ->
                    addStackEffect context <| functionTypeToStackEffects annotatedType

                Nothing ->
                    let
                        problem =
                            MissingTypeAnnotationInRecursiveCallStack
                                (Maybe.withDefault SourceLocation.emptyRange currentDef.sourceLocation)
                                currentDef.name
                    in
                    { context | errors = problem :: context.errors }

        Qualifier.Cycle _ data ->
            case TypeSignature.toMaybe data.typeSignature of
                Just annotatedType ->
                    addStackEffect context <| functionTypeToStackEffects annotatedType

                Nothing ->
                    let
                        problem =
                            MissingTypeAnnotationInRecursiveCallStack
                                (Maybe.withDefault SourceLocation.emptyRange data.sourceLocation)
                                data.name
                    in
                    { context | errors = problem :: context.errors }

        Qualifier.ConstructType typeDef ->
            let
                memberTypes =
                    getStructMembers typeDef
                        |> List.map Tuple.second

                typeInQuestion =
                    getStructType typeDef
            in
            addStackEffect context <|
                functionTypeToStackEffects
                    { input = memberTypes
                    , output = [ typeInQuestion ]
                    }

        Qualifier.SetMember typeDef _ memberType ->
            let
                typeInQuestion =
                    getStructType typeDef
            in
            addStackEffect context <|
                functionTypeToStackEffects
                    { input = [ typeInQuestion, memberType ]
                    , output = [ typeInQuestion ]
                    }

        Qualifier.GetMember typeDef _ memberType ->
            let
                typeInQuestion =
                    getStructType typeDef
            in
            addStackEffect context <|
                functionTypeToStackEffects
                    { input = [ typeInQuestion ]
                    , output = [ memberType ]
                    }

        Qualifier.Builtin _ builtin ->
            addStackEffect context <| functionTypeToStackEffects <| Builtin.functionType builtin


getStructMembers : TypeDefinition -> List ( String, Type )
getStructMembers typeDef =
    case typeDef.members of
        Qualifier.StructMembers members ->
            members

        Qualifier.UnionMembers _ ->
            []


getStructType : TypeDefinition -> Type
getStructType typeDef =
    let
        memberTypes =
            getStructMembers typeDef
                |> List.map Tuple.second

        genericMembers =
            List.filter Type.isGeneric memberTypes
    in
    case genericMembers of
        [] ->
            Type.Custom typeDef.name

        _ ->
            Type.CustomGeneric typeDef.name genericMembers


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


functionTypeToStackEffects : FunctionType -> List StackEffect
functionTypeToStackEffects functionType =
    List.map Pop (List.reverse functionType.input)
        ++ List.map Push functionType.output


functionTypeFromStackEffects : Qualifier.FunctionDefinition -> Context -> ( Context, FunctionType )
functionTypeFromStackEffects untypedDef context =
    functionTypeFromStackEffectsHelper
        untypedDef
        context.stackEffects
        ( context, { input = [], output = [] } )


functionTypeFromStackEffectsHelper : Qualifier.FunctionDefinition -> List StackEffect -> ( Context, FunctionType ) -> ( Context, FunctionType )
functionTypeFromStackEffectsHelper untypedDef effects ( context, functionType ) =
    let
        problem expected actual =
            UnexpectedType
                (Maybe.withDefault SourceLocation.emptyRange untypedDef.sourceLocation)
                untypedDef.name
                (resolveType context expected)
                (resolveType context actual)
    in
    case effects of
        [] ->
            ( context
            , { functionType
                | input = functionType.input
                , output = List.reverse functionType.output
              }
            )

        (Pop ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just needToPop ->
                    functionTypeFromStackEffectsHelper untypedDef (List.map Pop needToPop ++ remainingEffects) ( context, functionType )

                Nothing ->
                    case functionType.output of
                        [] ->
                            functionTypeFromStackEffectsHelper untypedDef remainingEffects <|
                                ( context, { functionType | input = type_ :: functionType.input } )

                        availableType :: remainingOutput ->
                            if availableType /= type_ then
                                ( { context | errors = problem type_ availableType :: context.errors }, functionType )

                            else
                                ( context, { functionType | output = remainingOutput } )

        (Pop type_) :: remainingEffects ->
            case functionType.output of
                [] ->
                    functionTypeFromStackEffectsHelper untypedDef remainingEffects <|
                        ( context, { functionType | input = type_ :: functionType.input } )

                availableType :: remainingOutput ->
                    let
                        ( newContext, compatible ) =
                            compatibleTypes context availableType type_
                    in
                    if not compatible then
                        ( { newContext | errors = problem type_ availableType :: context.errors }, functionType )

                    else
                        functionTypeFromStackEffectsHelper untypedDef remainingEffects <|
                            ( newContext, { functionType | output = remainingOutput } )

        (Push ((Type.StackRange rangeName) as type_)) :: remainingEffects ->
            case Dict.get rangeName context.boundStackRanges of
                Just range ->
                    functionTypeFromStackEffectsHelper untypedDef
                        (List.map Push range ++ remainingEffects)
                        ( context, functionType )

                Nothing ->
                    functionTypeFromStackEffectsHelper untypedDef remainingEffects <|
                        ( context, { functionType | output = type_ :: functionType.output } )

        (Push type_) :: remainingEffects ->
            functionTypeFromStackEffectsHelper untypedDef remainingEffects <|
                ( context, { functionType | output = type_ :: functionType.output } )


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

                    ( Type.Union _ unionTypes, rhs ) ->
                        -- Cannot normally go from union to concrete type
                        -- Special case: all generic members of union are bound to same type
                        case unionTypes of
                            [] ->
                                ( context, False )

                            firstType :: rest ->
                                let
                                    ( finalCtx, allBoundToSame ) =
                                        List.foldl helper ( context, True ) rest

                                    helper t ( ctx, oldTruth ) =
                                        let
                                            ( newCtx, thisTruth ) =
                                                compatibleTypes context firstType t
                                        in
                                        ( newCtx, oldTruth && thisTruth )
                                in
                                if allBoundToSame then
                                    compatibleTypes finalCtx firstType rhs

                                else
                                    ( context, False )

                    ( lhsType, Type.Union _ unionTypes ) ->
                        let
                            ( generics, nonGenerics ) =
                                List.partition Type.isGeneric unionTypes

                            ( nonGenericContext, compatibleNonGenericMember ) =
                                findCompatibleMember nonGenerics

                            findCompatibleMember members =
                                List.map (compatibleTypes context lhsType) members
                                    |> List.find Tuple.second
                                    |> Maybe.withDefault ( context, False )
                        in
                        if compatibleNonGenericMember then
                            ( nonGenericContext, True )

                        else
                            findCompatibleMember generics

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

        Type.Union name members ->
            members
                |> List.map (resolveType context)
                |> Type.Union name
                |> Just

        Type.CustomGeneric name members ->
            members
                |> List.map (resolveType context)
                |> Type.CustomGeneric name
                |> Just

        _ ->
            Just type_


resolveType : Context -> Type -> Type
resolveType context t =
    Maybe.withDefault t (getGenericBinding context t)


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


simplifyFunctionType : ( FunctionType, Context ) -> ( FunctionType, Context )
simplifyFunctionType ( functionType, context ) =
    let
        oldSignature =
            functionType.input ++ functionType.output

        inputLength =
            List.length functionType.input

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
    ( { input = List.take inputLength newSignature
      , output = List.drop inputLength newSignature
      }
    , context
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
