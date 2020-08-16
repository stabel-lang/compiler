module Play.Data.Metadata exposing (..)

import Play.Data.Type exposing (Type)
import Play.Data.TypeSignature exposing (TypeSignature(..))


type alias Metadata =
    { isEntryPoint : Bool
    , type_ : TypeSignature
    , isQuoted : Bool
    }


default : Metadata
default =
    { isEntryPoint = False
    , type_ = NotProvided
    , isQuoted = False
    }


asEntryPoint : Metadata -> Metadata
asEntryPoint meta =
    { meta | isEntryPoint = True }


withType : List Type -> List Type -> Metadata -> Metadata
withType inputs outputs meta =
    { meta | type_ = UserProvided { input = inputs, output = outputs } }


withVerifiedType : List Type -> List Type -> Metadata -> Metadata
withVerifiedType inputs outputs meta =
    { meta | type_ = CompilerProvided { input = inputs, output = outputs } }


isQuoted : Metadata -> Metadata
isQuoted meta =
    { meta | isQuoted = True }
