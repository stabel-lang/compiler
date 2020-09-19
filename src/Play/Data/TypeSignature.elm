module Play.Data.TypeSignature exposing (..)

import Play.Data.Type exposing (WordType)


type TypeSignature
    = NotProvided
    | UserProvided WordType
    | CompilerProvided WordType


map : (WordType -> WordType) -> TypeSignature -> TypeSignature
map fn ts =
    case ts of
        NotProvided ->
            NotProvided

        UserProvided wt ->
            UserProvided (fn wt)

        CompilerProvided wt ->
            CompilerProvided (fn wt)


toMaybe : TypeSignature -> Maybe WordType
toMaybe ts =
    case ts of
        NotProvided ->
            Nothing

        UserProvided wt ->
            Just wt

        CompilerProvided wt ->
            Just wt
