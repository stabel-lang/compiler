module Stabel.Parser.AssociatedFunctionSignature exposing
    ( AssociatedFunctionSignature(..)
    , toMaybe
    )

import Stabel.Parser.Type exposing (FunctionSignature)


type AssociatedFunctionSignature
    = NotProvided
    | UserProvided FunctionSignature
    | Verified FunctionSignature


toMaybe : AssociatedFunctionSignature -> Maybe FunctionSignature
toMaybe ts =
    case ts of
        NotProvided ->
            Nothing

        UserProvided wt ->
            Just wt

        Verified wt ->
            Just wt
