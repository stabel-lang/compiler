module Stabel.Data.Metadata exposing (..)

import Dict exposing (Dict)
import Stabel.Data.Type exposing (Type)
import Stabel.Data.TypeSignature exposing (TypeSignature(..))
import Stabel.Qualifier.SourceLocation exposing (SourceLocationRange)


type alias Metadata =
    { isEntryPoint : Bool
    , type_ : TypeSignature
    , isInline : Bool
    , sourceLocationRange : Maybe SourceLocationRange
    , isExposed : Bool
    }


default : Metadata
default =
    { isEntryPoint = False
    , type_ = NotProvided
    , isInline = False
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


isInline : Metadata -> Metadata
isInline meta =
    { meta | isInline = True }


withSourceLocationRange : SourceLocationRange -> Metadata -> Metadata
withSourceLocationRange range meta =
    { meta | sourceLocationRange = Just range }


clearSourceLocationRange : Metadata -> Metadata
clearSourceLocationRange meta =
    { meta | sourceLocationRange = Nothing }


isExposed : Bool -> Metadata -> Metadata
isExposed val meta =
    { meta | isExposed = val }
