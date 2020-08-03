module Play.Data.Type exposing (..)


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
