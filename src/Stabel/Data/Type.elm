module Stabel.Data.Type exposing
    ( FunctionType
    , Type(..)
    , compatibleFunctions
    , emptyFunctionType
    , functionTypeToString
    , genericName
    , genericlyCompatible
    , isGeneric
    , referencedGenerics
    , toDisplayString
    )

import Dict exposing (Dict)
import Set exposing (Set)


type Type
    = Int
    | Generic String
    | Custom String
    | CustomGeneric String (List Type)
    | Union (Maybe String) (List Type)
    | FunctionSignature FunctionType
    | StackRange String
    | Array Type


type alias FunctionType =
    { input : List Type
    , output : List Type
    }


emptyFunctionType : FunctionType
emptyFunctionType =
    { input = []
    , output = []
    }


isGeneric : Type -> Bool
isGeneric t =
    case t of
        Generic _ ->
            True

        _ ->
            False


genericName : Type -> Maybe String
genericName type_ =
    case type_ of
        Generic name ->
            Just name

        _ ->
            Nothing


referencedGenerics : Type -> Set String
referencedGenerics t =
    case t of
        Generic val ->
            Set.singleton val

        CustomGeneric _ members ->
            members
                |> List.map referencedGenerics
                |> List.foldl Set.union Set.empty

        Union _ members ->
            members
                |> List.map referencedGenerics
                |> List.foldl Set.union Set.empty

        _ ->
            Set.empty


genericlyCompatible : Type -> Type -> Bool
genericlyCompatible lhs rhs =
    case ( lhs, rhs ) of
        ( Generic _, Generic _ ) ->
            True

        ( Generic _, _ ) ->
            False

        ( _, Generic _ ) ->
            True

        ( CustomGeneric lName _, CustomGeneric rName _ ) ->
            lName == rName

        ( Union _ lMems, Union _ rMems ) ->
            lMems == rMems

        _ ->
            lhs == rhs


sameCategory : Type -> Type -> Bool
sameCategory lhs rhs =
    case ( lhs, rhs ) of
        ( FunctionSignature _, FunctionSignature _ ) ->
            True

        ( Union _ _, Union _ _ ) ->
            True

        _ ->
            lhs == rhs


toString : Type -> String
toString t =
    case t of
        Int ->
            "Int"

        Generic name ->
            name ++ "_Generic"

        Custom name ->
            name ++ "_Custom"

        CustomGeneric name _ ->
            name ++ "_Custom"

        Union (Just name) _ ->
            name ++ "_Union"

        Union Nothing _ ->
            "Union"

        FunctionSignature _ ->
            "Function"

        StackRange name ->
            name ++ "..."

        Array _ ->
            "Array"


toDisplayString : Type -> String
toDisplayString t =
    case t of
        Int ->
            "Int"

        Generic name ->
            name

        Custom name ->
            name

        CustomGeneric name [] ->
            name

        CustomGeneric name gens ->
            name ++ "(" ++ String.join ", " (List.map toDisplayString gens) ++ ")"

        Union (Just name) members ->
            let
                memberString =
                    members
                        |> List.map toDisplayString
                        |> String.join ", "
            in
            name
                ++ "("
                ++ memberString
                ++ ")"

        Union Nothing members ->
            toDisplayString (Union (Just "Union") members)

        FunctionSignature quotType ->
            "[ " ++ functionTypeToString quotType ++ " ]"

        StackRange name ->
            name ++ "..."

        Array t_ ->
            "Array(" ++ toDisplayString t_ ++ ")"


functionTypeToString : FunctionType -> String
functionTypeToString functionType =
    let
        inputTypeStrings =
            List.map toDisplayString functionType.input

        outputTypeStrings =
            List.map toDisplayString functionType.output
    in
    String.join " " inputTypeStrings ++ " -- " ++ String.join " " outputTypeStrings


compatibleFunctions : FunctionType -> FunctionType -> Bool
compatibleFunctions annotated inferred =
    let
        ( inputRangeDict, inputsCompatible ) =
            compatibleTypeLists annotated.input inferred.input Dict.empty

        ( _, outputsCompatible ) =
            compatibleTypeLists annotated.output inferred.output inputRangeDict
    in
    inputsCompatible && outputsCompatible


compatibleTypeLists : List Type -> List Type -> Dict String (List Type) -> ( Dict String (List Type), Bool )
compatibleTypeLists annotated inferred rangeDict =
    case ( annotated, inferred ) of
        ( [], [] ) ->
            ( rangeDict, True )

        ( (StackRange annotatedName) :: annotatedRest, (StackRange inferredName) :: inferredRest ) ->
            if annotatedName == inferredName then
                compatibleTypeLists annotatedRest inferredRest rangeDict

            else
                ( rangeDict, False )

        ( (StackRange _) :: [], [] ) ->
            ( rangeDict, True )

        ( [], (StackRange _) :: [] ) ->
            ( rangeDict, True )

        ( annotatedEl :: annotatedRest, (StackRange rangeName) :: [] ) ->
            compatibleTypeLists annotatedRest inferred <|
                Dict.update rangeName
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault []
                            |> (\existing -> existing ++ [ annotatedEl ])
                            |> Just
                    )
                    rangeDict

        ( annotatedEl :: annotatedRest, (StackRange rangeName) :: inferredNext :: inferredRest ) ->
            if sameCategory annotatedEl inferredNext then
                compatibleTypeLists annotated (inferredNext :: inferredRest) rangeDict

            else
                compatibleTypeLists annotatedRest inferred <|
                    Dict.update rangeName
                        (\maybeVal ->
                            maybeVal
                                |> Maybe.withDefault []
                                |> (\existing -> existing ++ [ annotatedEl ])
                                |> Just
                        )
                        rangeDict

        ( (FunctionSignature annotatedQuotType) :: annotatedRest, (FunctionSignature inferredQuotType) :: inferredRest ) ->
            let
                annotatedInputRangeApplied =
                    applyRangeDict rangeDict annotatedQuotType.input

                annotatedOutputRangeApplied =
                    applyRangeDict rangeDict annotatedQuotType.output

                inferredInputRangeApplied =
                    applyRangeDict rangeDict inferredQuotType.input

                inferredOutputRangeApplied =
                    applyRangeDict rangeDict inferredQuotType.output

                applyRangeDict rd types =
                    List.concatMap
                        (\type_ ->
                            case type_ of
                                StackRange rangeName ->
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
                    compatibleTypeLists annotatedInputRangeApplied inferredInputRangeApplied rangeDict

                ( dictRangePostOutputs, outputCompatible ) =
                    compatibleTypeLists annotatedOutputRangeApplied inferredOutputRangeApplied dictRangePostInputs
            in
            if inputCompatible && outputCompatible then
                compatibleTypeLists annotatedRest inferredRest dictRangePostOutputs

            else
                ( dictRangePostOutputs, False )

        ( (Generic _) :: annotatedRest, (Generic _) :: inferredRest ) ->
            compatibleTypeLists annotatedRest inferredRest rangeDict

        ( _ :: annotatedRest, (Generic _) :: inferredRest ) ->
            compatibleTypeLists annotatedRest inferredRest rangeDict

        ( (CustomGeneric lName lMembers) :: annotatedRest, (CustomGeneric rName rMembers) :: inferredRest ) ->
            let
                ( _, compatibleMembers ) =
                    compatibleTypeLists lMembers rMembers Dict.empty
            in
            if lName == rName && compatibleMembers then
                compatibleTypeLists annotatedRest inferredRest rangeDict

            else
                ( rangeDict, False )

        ( (Array lMember) :: annotatedRest, (Array rMember) :: inferredRest ) ->
            let
                ( _, compatibleMembers ) =
                    compatibleTypeLists [ lMember ] [ rMember ] Dict.empty
            in
            if compatibleMembers then
                ( rangeDict, True )

            else
                ( rangeDict, False )

        ( (Union _ lMembers) :: annotatedRest, (Union _ rMembers) :: inferredRest ) ->
            let
                lSet =
                    Set.fromList (List.map toString lMembers)

                rSet =
                    Set.fromList (List.map toString rMembers)

                diff =
                    Set.diff rSet lSet
                        |> Set.toList
            in
            case diff of
                [] ->
                    compatibleTypeLists annotatedRest inferredRest rangeDict

                [ oneDiff ] ->
                    -- Likely the default case
                    if String.endsWith "_Generic" oneDiff then
                        compatibleTypeLists annotatedRest inferredRest rangeDict

                    else
                        ( rangeDict, False )

                _ ->
                    ( rangeDict, False )

        ( (Union _ _) :: _, _ ) ->
            ( rangeDict, False )

        ( _, (Union _ _) :: _ ) ->
            ( rangeDict, False )

        ( annotatedEl :: annotatedRest, inferredEl :: inferredRest ) ->
            if annotatedEl == inferredEl then
                compatibleTypeLists annotatedRest inferredRest rangeDict

            else
                ( rangeDict, False )

        _ ->
            ( rangeDict, False )
