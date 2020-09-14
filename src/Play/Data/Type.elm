module Play.Data.Type exposing (..)

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


compatibleWords : WordType -> WordType -> Bool
compatibleWords annotated inferred =
    let
        ( inputRangeDict, inputsCompatible ) =
            compare annotated.input inferred.input Dict.empty

        ( _, outputsCompatible ) =
            compare annotated.output inferred.output inputRangeDict
    in
    inputsCompatible && outputsCompatible


compare : List Type -> List Type -> Dict String (List Type) -> ( Dict String (List Type), Bool )
compare lhs rhs rangeDict =
    case ( lhs, rhs ) of
        ( [], [] ) ->
            ( rangeDict, True )

        ( (StackRange lhsName) :: lhsRest, (StackRange rhsName) :: rhsRest ) ->
            if lhsName == rhsName then
                compare lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( (StackRange _) :: [], [] ) ->
            ( rangeDict, True )

        ( [], (StackRange _) :: [] ) ->
            ( rangeDict, True )

        ( lhsEl :: lhsRest, (StackRange rangeName) :: [] ) ->
            compare lhsRest rhs <|
                Dict.update rangeName
                    (\maybeVal ->
                        maybeVal
                            |> Maybe.withDefault []
                            |> (\existing -> existing ++ [ lhsEl ])
                            |> Just
                    )
                    rangeDict

        ( lhsEl :: lhsRest, (StackRange rangeName) :: rhsNext :: rhsRest ) ->
            if sameCategory lhsEl rhsNext then
                compare lhs (rhsNext :: rhsRest) rangeDict

            else
                compare lhsRest rhs <|
                    Dict.update rangeName
                        (\maybeVal ->
                            maybeVal
                                |> Maybe.withDefault []
                                |> (\existing -> existing ++ [ lhsEl ])
                                |> Just
                        )
                        rangeDict

        ( (Quotation lhsQuotType) :: lhsRest, (Quotation rhsQuotType) :: rhsRest ) ->
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
                    compare lhsInputRangeApplied rhsInputRangeApplied rangeDict

                ( dictRangePostOutputs, outputCompatible ) =
                    compare lhsOutputRangeApplied rhsOutputRangeApplied dictRangePostInputs
            in
            if inputCompatible && outputCompatible then
                compare lhsRest rhsRest dictRangePostOutputs

            else
                ( dictRangePostOutputs, False )

        ( (Generic _) :: lhsRest, _ :: rhsRest ) ->
            compare lhsRest rhsRest rangeDict

        ( _ :: lhsRest, (Generic _) :: rhsRest ) ->
            compare lhsRest rhsRest rangeDict

        ( (CustomGeneric lName lMembers) :: lhsRest, (CustomGeneric rName rMembers) :: rhsRest ) ->
            let
                ( _, compatibleMembers ) =
                    compare lMembers rMembers Dict.empty
            in
            if lName == rName && compatibleMembers then
                compare lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( (Union lMembers) :: lhsRest, (Union rMembers) :: rhsRest ) ->
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
                    compare lhsRest rhsRest rangeDict

                [ oneDiff ] ->
                    -- Likely the default case
                    if String.endsWith "_Generic" oneDiff then
                        compare lhsRest rhsRest rangeDict

                    else
                        ( rangeDict, False )

                _ ->
                    ( rangeDict, False )

        ( (Union _) :: _, _ ) ->
            -- Cannot go from union to concrete type
            ( rangeDict, False )

        ( lhsEl :: lhsRest, (Union rMembers) :: rhsRest ) ->
            let
                compatible =
                    rMembers
                        |> List.map toString
                        |> Set.fromList
                        |> Set.member (toString lhsEl)
            in
            if compatible then
                compare lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        ( lhsEl :: lhsRest, rhsEl :: rhsRest ) ->
            if lhsEl == rhsEl then
                compare lhsRest rhsRest rangeDict

            else
                ( rangeDict, False )

        _ ->
            ( rangeDict, False )
