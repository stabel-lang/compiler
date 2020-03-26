module Play.Data.Type exposing (..)


type Type
    = Int


type alias WordType =
    { input : List Type
    , output : List Type
    }
