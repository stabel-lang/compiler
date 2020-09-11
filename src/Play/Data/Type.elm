module Play.Data.Type exposing (..)

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
