module Play.Data.Metadata exposing (..)

import Dict exposing (Dict)
import Play.Data.SourceLocation exposing (SourceLocationRange)
import Play.Data.Type exposing (Type)
import Play.Data.TypeSignature exposing (TypeSignature(..))


type alias Metadata =
    { isEntryPoint : Bool
    , type_ : TypeSignature
    , isQuoted : Bool
    , sourceLocationRange : Maybe SourceLocationRange
    , isExposed : Bool
    }


default : Metadata
default =
    { isEntryPoint = False
    , type_ = NotProvided
    , isQuoted = False
    , sourceLocationRange = Nothing
    , isExposed = True
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


withSourceLocationRange : SourceLocationRange -> Metadata -> Metadata
withSourceLocationRange range meta =
    { meta | sourceLocationRange = Just range }


clearSourceLocationRange : Metadata -> Metadata
clearSourceLocationRange meta =
    { meta | sourceLocationRange = Nothing }


isExposed : Bool -> Metadata -> Metadata
isExposed val meta =
    { meta | isExposed = val }
