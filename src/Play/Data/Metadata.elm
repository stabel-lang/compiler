module Play.Data.Metadata exposing (..)

import Play.Data.Type exposing (WordType)


type alias Metadata =
    { isEntryPoint : Bool
    , type_ : Maybe WordType
    }


default : Metadata
default =
    { isEntryPoint = False
    , type_ = Nothing
    }
