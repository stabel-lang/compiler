module Play.Data.Type exposing (..)


type Type
    = Int
    | Custom String
    | Generic String
    | Union (List Type)
    | Quotation WordType
    | StackRange String


type alias WordType =
    { input : List Type
    , output : List Type
    }
