module Play.Data.Metadata exposing (..)


type alias Metadata =
    { isEntryPoint : Bool
    }


default : Metadata
default =
    { isEntryPoint = False
    }
