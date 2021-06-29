module Stabel.Data.TypeSignature exposing (..)

import Stabel.Data.Type exposing (FunctionType)


type TypeSignature
    = NotProvided
    | UserProvided FunctionType
    | CompilerProvided FunctionType


map : (FunctionType -> FunctionType) -> TypeSignature -> TypeSignature
map fn ts =
    case ts of
        NotProvided ->
            NotProvided

        UserProvided wt ->
            UserProvided (fn wt)

        CompilerProvided wt ->
            CompilerProvided (fn wt)


toMaybe : TypeSignature -> Maybe FunctionType
toMaybe ts =
    case ts of
        NotProvided ->
            Nothing

        UserProvided wt ->
            Just wt

        CompilerProvided wt ->
            Just wt
