module Play.PackageLoader exposing (init)

import Json.Decode as Json


type Problem
    = InvalidPackageMetadata String String


type alias State =
    ()


type SideEffect
    = NoOp


init : String -> Json.Value -> Result Problem ( State, SideEffect )
init jsonFilePath json =
    Err <| InvalidPackageMetadata jsonFilePath "todo"
