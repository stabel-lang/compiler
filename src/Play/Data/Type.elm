module Play.Data.Type exposing
    ( Type(..)
    , WordType
    , compatibleWords
    , genericName
    , genericlyCompatible
    , isGeneric
    , referencedGenerics
    , toDisplayString
    , wordTypeToString
    )

import Dict exposing (Dict)
import Set exposing (Set)


type Type
    = Int
    | Generic String
    | Custom String
    | CustomGeneric String (List Type)
    | Union (List Type)
    | Quotation WordType
    | StackRange String


type alias WordType =
    { input : List Type
    , output : List Type
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

        Union members ->
            members
                |> List.map referencedGenerics
                |> List.foldl Set.union Set.empty

        _ ->
            Set.empty


genericlyCompatible : Type -> Type -> Bool
genericlyCompatible lhs rhs =
    case ( lhs, rhs ) of
        ( Generic _, _ ) ->
            True

        ( _, Generic _ ) ->
            True

        ( CustomGeneric lName _, CustomGeneric rName _ ) ->
            lName == rName

        _ ->
            lhs == rhs


sameCategory : Type -> Type -> Bool
sameCategory lhs rhs =
    case ( lhs, rhs ) of
        ( Quotation _, Quotation _ ) ->
            True

        ( Union _, Union _ ) ->
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

        Union _ ->
            "Union"

        Quotation _ ->
            "quot"

        StackRange name ->
            name ++ "..."


toDisplayString : Type -> String
toDisplayString t =
    case t of
        Int ->
            "Int"

        Generic name ->
            name

        Custom name ->
            name

        CustomGeneric name _ ->
            name

        Union _ ->
            "Union"

        Quotation quotType ->
            "[ " ++ wordTypeToString quotType ++ " ]"

        StackRange name ->
            name ++ "..."


wordTypeToString : WordType -> String
wordTypeToString wordType =
    let
        inputTypeStrings =
            List.map toDisplayString wordType.input

        outputTypeStrings =
            List.map toDisplayString wordType.output
    in
    String.join " " inputTypeStrings ++ " -- " ++ String.join " " outputTypeStrings


compatibleWords : WordType -> WordType -> Bool
compatibleWords annotated inferred =
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

        ( (Quotation annotatedQuotType) :: annotatedRest, (Quotation inferredQuotType) :: inferredRest ) ->
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

        ( (Union lMembers) :: annotatedRest, (Union rMembers) :: inferredRest ) ->
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

        ( (Union _) :: _, _ ) ->
            -- Cannot go from union to concrete type
            ( rangeDict, False )

        ( annotatedEl :: annotatedRest, (Union rMembers) :: inferredRest ) ->
            let
                compatible =
                    rMembers
                        |> List.map toString
                        |> Set.fromList
                        |> Set.member (toString annotatedEl)
            in
            if compatible then
                compatibleTypeLists annotatedRest inferredRest rangeDict

            else
                ( rangeDict, False )

        ( annotatedEl :: annotatedRest, inferredEl :: inferredRest ) ->
            if annotatedEl == inferredEl then
                compatibleTypeLists annotatedRest inferredRest rangeDict

            else
                ( rangeDict, False )

        _ ->
            ( rangeDict, False )
