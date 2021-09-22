module Stabel.TypeChecker exposing
    ( AST
    , AstNode(..)
    , CycleData
    , FunctionDefinition
    , FunctionImplementation(..)
    , TypeDefinition
    , TypeMatch(..)
    , run
    )

import Dict exposing (Dict)
import List.Extra as List
import Result.Extra as Result
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
    , referencableFunctions : Set String
    }


type alias TypeDefinition =
    Qualifier.TypeDefinition


type alias FunctionDefinition =
    { name : String
    , sourceLocation : Maybe SourceLocationRange
    , type_ : FunctionType
    , implementation : FunctionImplementation
    }


type FunctionImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatchInt SourceLocationRange Int
    | TypeMatchType SourceLocationRange Type (List ( String, TypeMatch ))


type AstNode
    = IntLiteral SourceLocationRange Int
    | ArrayLiteral SourceLocationRange (List AstNode) Type
    | Function SourceLocationRange FunctionDefinition FunctionType
    | FunctionRef SourceLocationRange FunctionDefinition
    | Recurse SourceLocationRange
    | Cycle SourceLocationRange CycleData
    | Builtin SourceLocationRange Builtin
    | ConstructType TypeDefinition
    | SetMember TypeDefinition String Int Type
    | GetMember TypeDefinition String Int Type


type alias CycleData =
    { name : String
    , sourceLocation : Maybe SourceLocationRange
    , typeSignature : FunctionType
    , isMultiFunction : Bool
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
        |> List.map (\gen -> UndeclaredGeneric range gen)


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
            Dict.foldl
                (\_ v acc -> Tuple.second <| typeCheckDefinition v acc)
                context
                ast.functions
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = updatedContext.types
            , functions = updatedContext.typedFunctions
            , referencableFunctions = ast.referenceableFunctions
            }

    else
        Err updatedContext.errors


typeCheckDefinition : Qualifier.FunctionDefinition -> Context -> ( FunctionDefinition, Context )
typeCheckDefinition untypedDef context =
    case Dict.get untypedDef.name context.typedFunctions of
        Just def ->
            ( def, context )

        Nothing ->
            case untypedDef.implementation of
                Qualifier.SoloImpl impl ->
                    typeCheckSoloImplementation
                        context
                        untypedDef
                        impl

                Qualifier.MultiImpl initialWhens defaultImpl ->
                    typeCheckMultiImplementation
                        context
                        untypedDef
                        initialWhens
                        defaultImpl



-- Type Check Solo Impl --


typeCheckSoloImplementation : Context -> Qualifier.FunctionDefinition -> List Qualifier.Node -> ( FunctionDefinition, Context )
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

        typedDef =
            { name = untypedDef.name
            , sourceLocation = untypedDef.sourceLocation
            , type_ =
                untypedDef.typeSignature
                    |> TypeSignature.withDefault inferredType
            , implementation = typedImplementation
            }

        finalContext =
            { newContext
                | typedFunctions =
                    Dict.insert untypedDef.name typedDef newContext.typedFunctions
            }
                |> verifyTypeSignature inferredType untypedDef
                |> cleanContext
    in
    ( typedDef
    , finalContext
    )


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

        Qualifier.ArrayLiteral range nodes ->
            let
                typedNodes =
                    List.map (untypedToTypedNode idx context) nodes

                arrayType =
                    List.filterMap arrayNodeToType typedNodes
                        |> unionizeTypes
                        |> Type.Array
            in
            ArrayLiteral range typedNodes arrayType

        Qualifier.Function range function ->
            let
                ( def, _ ) =
                    typeCheckDefinition function context
            in
            Function range def <|
                resolveGenericsInFunctionType idx context def.type_

        Qualifier.FunctionRef range ref ->
            let
                ( def, _ ) =
                    typeCheckDefinition ref context
            in
            FunctionRef range def

        Qualifier.Recurse range ->
            Recurse range

        Qualifier.Cycle range data ->
            let
                functionType =
                    data.typeSignature
                        |> TypeSignature.map (resolveGenericsInFunctionType idx context)
                        |> TypeSignature.withDefault Type.emptyFunctionType
            in
            Cycle range
                { name = data.name
                , sourceLocation = data.sourceLocation
                , typeSignature = functionType
                , isMultiFunction = data.isMultiFunction
                }

        Qualifier.Builtin range builtin ->
            Builtin range builtin

        Qualifier.ConstructType typeDef ->
            ConstructType typeDef

        Qualifier.SetMember typeDef memberName memberIndex memberType ->
            SetMember typeDef memberName memberIndex memberType

        Qualifier.GetMember typeDef memberName memberIndex memberType ->
            GetMember typeDef memberName memberIndex memberType


arrayNodeToType : AstNode -> Maybe Type
arrayNodeToType n =
    case n of
        IntLiteral _ _ ->
            Just Type.Int

        ArrayLiteral _ _ t ->
            Just t

        Function _ _ t ->
            List.head t.output

        FunctionRef _ def ->
            Just <| Type.FunctionSignature def.type_

        Recurse _ ->
            Nothing

        Cycle _ data ->
            List.head data.typeSignature.output

        Builtin _ _ ->
            Nothing

        ConstructType _ ->
            Nothing

        SetMember _ _ _ _ ->
            Nothing

        GetMember _ _ _ _ ->
            Nothing


resolveGenericsInFunctionType : Int -> Context -> FunctionType -> FunctionType
resolveGenericsInFunctionType idx context wt =
    let
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

                Type.CustomGeneric name members ->
                    Type.CustomGeneric name <|
                        List.map replaceGenericWithBoundValue members

                _ ->
                    boundType
    in
    { input =
        List.map (tagGeneric idx >> replaceGenericWithBoundValue) wt.input
    , output =
        List.map (tagGeneric idx >> replaceGenericWithBoundValue) wt.output
    }



-- Type Check Multi functions --


typeCheckMultiImplementation :
    Context
    -> Qualifier.FunctionDefinition
    -> List ( Qualifier.TypeMatch, List Qualifier.Node )
    -> List Qualifier.Node
    -> ( FunctionDefinition, Context )
typeCheckMultiImplementation context untypedDef initialWhens defaultImpl =
    let
        allBranches =
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
                            ( Qualifier.TypeMatchType SourceLocation.emptyRange (Type.Generic "*") [], defaultImpl ) :: initialWhens

                        firstType :: _ ->
                            ( Qualifier.TypeMatchType SourceLocation.emptyRange firstType [], defaultImpl ) :: initialWhens

        whens =
            List.map (Tuple.mapFirst (resolveWhenConditions untypedDef)) allBranches

        ( inferredWhenTypes, newContext ) =
            whens
                |> List.foldr (inferWhenTypes untypedDef) ( [], context )
                |> Tuple.mapFirst normalizeWhenTypes
                |> simplifyWhenFunctionTypes
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

        typedDef =
            { name = untypedDef.name
            , sourceLocation = untypedDef.sourceLocation
            , type_ = exposedType
            , implementation =
                MultiImpl
                    (List.map
                        (Tuple.mapBoth mapTypeMatch typeImplementation)
                        initialWhens
                    )
                    (typeImplementation defaultImpl)
            }

        typeImplementation impl =
            untypedToTypedImplementation newContext impl

        finalContext =
            { newContext
                | typedFunctions =
                    Dict.insert untypedDef.name typedDef newContext.typedFunctions
                , errors =
                    List.filterMap identity
                        [ maybeConsistencyError
                        , maybeInexhaustiveError
                        ]
                        ++ newContext.errors
            }
                |> verifyTypeSignature inferredType untypedDef
                |> cleanContext
    in
    ( typedDef
    , finalContext
    )


resolveWhenConditions : Qualifier.FunctionDefinition -> Qualifier.TypeMatch -> Qualifier.TypeMatch
resolveWhenConditions untypedDef match =
    case match of
        Qualifier.TypeMatchType loc ((Type.CustomGeneric _ structGenerics) as t) conds ->
            let
                bindings =
                    List.foldl
                        whenConditionsGenericBindings
                        (initialBindingsFromFunctionDef untypedDef structGenerics)
                        conds
            in
            Qualifier.TypeMatchType
                loc
                (bindGenericsInType bindings t)
                (List.map (whenConditionsBindGenerics bindings) conds)

        _ ->
            match


initialBindingsFromFunctionDef : Qualifier.FunctionDefinition -> List Type -> Dict String Type
initialBindingsFromFunctionDef def structGenerics =
    case TypeSignature.toMaybe def.typeSignature of
        Just wt ->
            case wt.input of
                (Type.CustomGeneric _ possiblyBoundGenerics) :: _ ->
                    let
                        liftTupleMaybe ( first, second ) =
                            Maybe.map (\x -> ( x, second )) first
                    in
                    List.map2 Tuple.pair structGenerics possiblyBoundGenerics
                        |> List.map (Tuple.mapFirst Type.genericName)
                        |> List.filterMap liftTupleMaybe
                        |> Dict.fromList

                _ ->
                    Dict.empty

        Nothing ->
            Dict.empty


whenConditionsGenericBindings : ( String, Type, Qualifier.TypeMatch ) -> Dict String Type -> Dict String Type
whenConditionsGenericBindings ( _, fieldType, value ) bindings =
    case ( fieldType, value ) of
        ( Type.Generic genericName, Qualifier.TypeMatchInt _ _ ) ->
            Dict.insert genericName Type.Int bindings

        ( Type.Generic genericName, Qualifier.TypeMatchType _ t [] ) ->
            Dict.insert genericName t bindings

        ( _, Qualifier.TypeMatchType _ _ subConds ) ->
            List.foldl whenConditionsGenericBindings bindings subConds

        _ ->
            bindings


whenConditionsBindGenerics : Dict String Type -> ( String, Type, Qualifier.TypeMatch ) -> ( String, Type, Qualifier.TypeMatch )
whenConditionsBindGenerics bindings ( fieldName, fieldType, value ) =
    let
        boundValue =
            case value of
                Qualifier.TypeMatchType subLoc subType subConds ->
                    Qualifier.TypeMatchType
                        subLoc
                        (bindGenericsInType bindings subType)
                        (List.map (whenConditionsBindGenerics bindings) subConds)

                _ ->
                    value
    in
    ( fieldName
    , bindGenericsInType bindings fieldType
    , boundValue
    )


bindGenericsInType : Dict String Type -> Type -> Type
bindGenericsInType bindings t =
    case t of
        Type.Generic genericName ->
            Dict.get genericName bindings
                |> Maybe.withDefault t

        Type.CustomGeneric name gens ->
            Type.CustomGeneric name <|
                List.map (bindGenericsInType bindings) gens

        Type.Union name members ->
            Type.Union name <|
                List.map (bindGenericsInType bindings) members

        _ ->
            t


inferWhenTypes :
    Qualifier.FunctionDefinition
    -> ( Qualifier.TypeMatch, List Qualifier.Node )
    -> ( List FunctionType, Context )
    -> ( List FunctionType, Context )
inferWhenTypes untypedDef ( typeMatch, im ) ( infs, ctx ) =
    let
        t =
            typeOfTypeMatch typeMatch

        alteredTypeSignature =
            case untypedDef.typeSignature of
                TypeSignature.UserProvided wt ->
                    TypeSignature.UserProvided <|
                        case wt.input of
                            firstAnnotatedType :: rest ->
                                { wt | input = resolveFirstType firstAnnotatedType t :: rest }

                            _ ->
                                wt

                x ->
                    x

        ( inf, newCtx ) =
            typeCheckImplementation untypedDef alteredTypeSignature im (cleanContext ctx)
    in
    ( inf :: infs, newCtx )


typeOfTypeMatch : Qualifier.TypeMatch -> Type
typeOfTypeMatch typeMatch =
    case typeMatch of
        Qualifier.TypeMatchInt _ _ ->
            Type.Int

        Qualifier.TypeMatchType _ t _ ->
            t


resolveFirstType : Type -> Type -> Type
resolveFirstType annotatedType typeMatchType =
    case ( annotatedType, typeMatchType ) of
        ( Type.Union _ unionMembers, Type.CustomGeneric name _ ) ->
            List.find (matchingCustomGenericType name) unionMembers
                |> Maybe.withDefault typeMatchType

        ( Type.CustomGeneric annName _, Type.CustomGeneric matchName _ ) ->
            if annName == matchName then
                annotatedType

            else
                typeMatchType

        _ ->
            typeMatchType


matchingCustomGenericType : String -> Type -> Bool
matchingCustomGenericType nameToMatch tipe =
    case tipe of
        Type.CustomGeneric name _ ->
            name == nameToMatch

        _ ->
            False


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


simplifyWhenFunctionTypes : ( List FunctionType, Context ) -> ( List FunctionType, Context )
simplifyWhenFunctionTypes ( functionTypes, context ) =
    ( List.map
        (\wt -> Tuple.first (simplifyFunctionType ( wt, context )))
        functionTypes
    , context
    )


replaceFirstTypeWithPatternMatch : ( Qualifier.TypeMatch, FunctionType ) -> FunctionType
replaceFirstTypeWithPatternMatch ( typeMatch, typeSignature ) =
    let
        matchType =
            typeOfTypeMatch typeMatch
    in
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
        |> Tuple.mapSecond (\lobotomizedFunctionTypes -> equalizeWhenTypesHelper lobotomizedFunctionTypes [])
        |> (\( firstTypes, equalizedWhenTypes ) -> List.map2 Tuple.pair firstTypes equalizedWhenTypes)
        |> List.map joinSplitFunctionType


equalizeWhenTypesHelper : List FunctionType -> List FunctionType -> List FunctionType
equalizeWhenTypesHelper types acc =
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
                (newSecondType :: newFirstType :: acc)


unionOfTypeMatches : List ( Qualifier.TypeMatch, a ) -> Type
unionOfTypeMatches whenBranches =
    let
        uniqueTypes =
            whenBranches
                |> List.map (Tuple.first >> typeOfTypeMatch)
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

        ( annotatedEl :: annotatedRest, (Type.StackRange infGen) :: inferredRest ) ->
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
patternMatchIsCompatibleWithInferredType ( typeMatch, inf ) =
    case inf.input of
        inferredType :: _ ->
            Type.genericlyCompatible (typeOfTypeMatch typeMatch) inferredType

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


mapTypeMatch : Qualifier.TypeMatch -> TypeMatch
mapTypeMatch typeMatch =
    case typeMatch of
        Qualifier.TypeMatchInt range val ->
            TypeMatchInt range val

        Qualifier.TypeMatchType range type_ cond ->
            TypeMatchType range type_ (List.map (mapQualifiedMatch mapTypeMatch) cond)


mapQualifiedMatch : (Qualifier.TypeMatch -> TypeMatch) -> ( String, Type, Qualifier.TypeMatch ) -> ( String, TypeMatch )
mapQualifiedMatch fn ( fieldName, _, match ) =
    ( fieldName, fn match )


type InexhaustiveState
    = Total
    | SeenInt


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


inexhaustivenessCheckHelper : List Type -> Qualifier.TypeMatch -> List ( List Type, InexhaustiveState ) -> List ( List Type, InexhaustiveState )
inexhaustivenessCheckHelper typePrefix typeMatch acc =
    let
        ( t, conds ) =
            case typeMatch of
                Qualifier.TypeMatchInt _ _ ->
                    ( Type.Int, [] )

                Qualifier.TypeMatchType _ t_ conds_ ->
                    ( t_, conds_ )

        typeList =
            typePrefix ++ [ t ]
    in
    if List.any (\( toMatch, state ) -> typeList == toMatch && state == Total) acc then
        acc

    else
        let
            subcases =
                conds
                    |> List.filterMap isRecursiveMatch
                    |> List.foldl (inexhaustivenessCheckHelper typeList) acc

            isRecursiveMatch ( _, _, match ) =
                case match of
                    Qualifier.TypeMatchType _ _ (_ :: _) ->
                        Just match

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
                    simplifyFunctionTypeGenerics annotatedType
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
    case nodeToStackEffect currentDef node context of
        Ok ( newContext, effects ) ->
            addStackEffect newContext idx effects

        Err error ->
            { context | errors = error :: context.errors }


nodeToStackEffect : Qualifier.FunctionDefinition -> Qualifier.Node -> Context -> Result Problem ( Context, List StackEffect )
nodeToStackEffect currentDef node context =
    case node of
        Qualifier.Integer _ _ ->
            Ok ( context, [ Push Type.Int ] )

        Qualifier.ArrayLiteral loc nodes ->
            let
                res =
                    List.foldr
                        (\n acc ->
                            case acc of
                                Err _ ->
                                    acc

                                Ok ( previousStackEffects, previousContext ) ->
                                    case nodeToStackEffect currentDef n previousContext of
                                        Err err ->
                                            Err err

                                        Ok ( nextContext, nodeStackEffects ) ->
                                            Ok ( nodeStackEffects :: previousStackEffects, nextContext )
                        )
                        (Ok ( [], context ))
                        nodes
                        |> Result.map (Tuple.mapFirst (List.map effectsToFunctionType))
                        |> Result.map (Tuple.mapFirst (List.map validateArrayType))
                        |> Result.map (Tuple.mapFirst Result.combine)
                        |> Result.andThen liftTupleFirstResult
                        |> Result.map (Tuple.mapFirst (List.concatMap .output))
                        |> Result.map (Tuple.mapFirst unionizeTypes)

                effectsToFunctionType effects =
                    let
                        ( inputs, outputs ) =
                            List.partition stackEffectIsPop effects
                                |> Tuple.mapBoth (List.map stackEffectType) (List.map stackEffectType)
                    in
                    { input = inputs
                    , output = outputs
                    }

                stackEffectIsPop effect =
                    case effect of
                        Pop _ ->
                            True

                        Push _ ->
                            False

                stackEffectType effect =
                    case effect of
                        Pop t ->
                            t

                        Push t ->
                            t

                validateArrayType type_ =
                    if List.isEmpty type_.input && List.length type_.output == 1 then
                        Ok type_

                    else
                        Err <| BadArrayElement loc type_

                liftTupleFirstResult ( resA, b ) =
                    case resA of
                        Ok a ->
                            Ok ( a, b )

                        Err err ->
                            Err err
            in
            case res of
                Ok ( inferredType, newContext ) ->
                    Ok ( newContext, [ Push <| Type.Array inferredType ] )

                Err err ->
                    Err err

        Qualifier.Function _ untypedDef ->
            let
                ( def, contextWithTypedDef ) =
                    typeCheckDefinition untypedDef context

                newContext =
                    { contextWithTypedDef | stackEffects = context.stackEffects }
            in
            Ok ( newContext, functionTypeToStackEffects def.type_ )

        Qualifier.FunctionRef _ ref ->
            let
                stackEffectsBeforeFunctionCheck =
                    context.stackEffects

                ( def, contextAfterFunctionCheck ) =
                    typeCheckDefinition ref context

                newContext =
                    { contextAfterFunctionCheck | stackEffects = stackEffectsBeforeFunctionCheck }
            in
            Ok
                ( newContext
                , [ Push <| Type.FunctionSignature def.type_ ]
                )

        Qualifier.Recurse _ ->
            case TypeSignature.toMaybe currentDef.typeSignature of
                Just annotatedType ->
                    Ok ( context, functionTypeToStackEffects annotatedType )

                Nothing ->
                    Err <|
                        MissingTypeAnnotationInRecursiveCallStack
                            (Maybe.withDefault SourceLocation.emptyRange currentDef.sourceLocation)
                            currentDef.name

        Qualifier.Cycle _ data ->
            case TypeSignature.toMaybe data.typeSignature of
                Just annotatedType ->
                    Ok ( context, functionTypeToStackEffects annotatedType )

                Nothing ->
                    Err <|
                        MissingTypeAnnotationInRecursiveCallStack
                            (Maybe.withDefault SourceLocation.emptyRange data.sourceLocation)
                            data.name

        Qualifier.ConstructType typeDef ->
            let
                memberTypes =
                    getStructMembers typeDef
                        |> List.map Tuple.second

                typeInQuestion =
                    getStructType typeDef
            in
            Ok
                ( context
                , functionTypeToStackEffects
                    { input = memberTypes
                    , output = [ typeInQuestion ]
                    }
                )

        Qualifier.SetMember typeDef _ _ memberType ->
            let
                typeInQuestion =
                    getStructType typeDef
            in
            Ok
                ( context
                , functionTypeToStackEffects
                    { input = [ typeInQuestion, memberType ]
                    , output = [ typeInQuestion ]
                    }
                )

        Qualifier.GetMember typeDef _ _ memberType ->
            let
                typeInQuestion =
                    getStructType typeDef
            in
            Ok
                ( context
                , functionTypeToStackEffects
                    { input = [ typeInQuestion ]
                    , output = [ memberType ]
                    }
                )

        Qualifier.Builtin _ builtin ->
            Ok
                ( context
                , functionTypeToStackEffects <|
                    Builtin.functionType builtin
                )


addStackEffect : Context -> Int -> List StackEffect -> Context
addStackEffect ctx idx effects =
    { ctx | stackEffects = ctx.stackEffects ++ List.map (tagGenericEffect idx) effects }


unionizeTypes : List Type -> Type
unionizeTypes ts =
    unionizeTypesHelper ts []


unionizeTypesHelper : List Type -> List Type -> Type
unionizeTypesHelper ts acc =
    case ts of
        [] ->
            case acc of
                [] ->
                    Type.Generic "a"

                [ t ] ->
                    t

                _ ->
                    Type.Union Nothing acc

        t :: rest ->
            if List.member t acc then
                unionizeTypesHelper rest acc

            else
                unionizeTypesHelper rest (t :: acc)


getStructMembers : TypeDefinition -> List ( String, Type )
getStructMembers typeDef =
    case typeDef.members of
        Qualifier.StructMembers members ->
            members

        Qualifier.UnionMembers _ ->
            []


getStructType : TypeDefinition -> Type
getStructType typeDef =
    case typeDef.generics of
        [] ->
            Type.Custom typeDef.name

        gens ->
            Type.CustomGeneric typeDef.name (List.map Type.Generic gens)


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

        Type.Array t ->
            Type.Array <| tagGeneric idx t

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
                | output = List.reverse functionType.output
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
                        let
                            -- TODO: lift this restriction in the future?
                            lengthTest =
                                List.length leftUnion == List.length rightUnion

                            ( newContext, allMembersTest ) =
                                subList leftUnion rightUnion context
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
                                                compatibleTypes ctx firstType t
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

                    ( Type.Array lt, Type.Array rt ) ->
                        compatibleTypes context lt rt

                    _ ->
                        ( context, False )


{-| Is the first list a subset of the second?
-}
subList : List Type -> List Type -> Context -> ( Context, Bool )
subList lhs rhs ctx =
    case lhs of
        [] ->
            ( ctx, True )

        first :: rest ->
            case findMap (compatibleTypes ctx first) Tuple.second rhs of
                Just ( rhsType, ( newContext, _ ) ) ->
                    subList
                        rest
                        (List.filter ((/=) rhsType) rest)
                        newContext

                Nothing ->
                    ( ctx, False )


findMap : (a -> b) -> (b -> Bool) -> List a -> Maybe ( a, b )
findMap mapFn predFn ls =
    case ls of
        [] ->
            Nothing

        first :: rest ->
            let
                mapped =
                    mapFn first
            in
            if predFn mapped then
                Just ( first, mapped )

            else
                findMap mapFn predFn rest


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
    in
    ( { input = List.take inputLength newSignature
      , output = List.drop inputLength newSignature
      }
    , context
    )


simplifyFunctionTypeGenerics : FunctionType -> FunctionType
simplifyFunctionTypeGenerics functionType =
    let
        oldSignature =
            functionType.input ++ functionType.output

        inputLength =
            List.length functionType.input

        newSignature =
            oldSignature
                |> List.foldl renameGenerics ( 'a', Dict.empty, [] )
                |> (\( _, _, ns ) -> ns)
                |> List.reverse
    in
    { input = List.take inputLength newSignature
    , output = List.drop inputLength newSignature
    }


renameGenerics :
    Type
    -> ( Char, Dict String String, List Type )
    -> ( Char, Dict String String, List Type )
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
