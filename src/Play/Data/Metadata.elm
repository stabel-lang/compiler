module Play.Data.Metadata exposing (..)

import Play.Data.Type exposing (Type, WordType)


type alias Metadata =
    { isEntryPoint : Bool
    , type_ : Maybe WordType
    , isQuoted : Bool
    }


default : Metadata
default =
    { isEntryPoint = False
    , type_ = Nothing
    , isQuoted = False
    }


asEntryPoint : Metadata -> Metadata
asEntryPoint meta =
    { meta | isEntryPoint = True }


withType : List Type -> List Type -> Metadata -> Metadata
withType inputs outputs meta =
    { meta | type_ = Just { input = inputs, output = outputs } }


isQuoted : Metadata -> Metadata
isQuoted meta =
    { meta | isQuoted = True }
