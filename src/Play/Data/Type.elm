module Play.Data.Type exposing (..)


type Type
    = Int
    | Custom String


type alias WordType =
    { input : List Type
    , output : List Type
    }
