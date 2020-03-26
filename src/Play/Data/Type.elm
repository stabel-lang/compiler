module Play.Data.Type exposing (..)


type Type
    = IntType


type alias WordType =
    { input : List Type
    , output : List Type
    }
