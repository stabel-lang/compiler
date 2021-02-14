module Play.PackageLoader exposing (init)

import Json.Decode as Json
import Play.Data.PackageMetadata as PackageMetadata


type Problem
    = InvalidPackageMetadata String String


type alias State =
    ()


type SideEffect
    = NoOp


init : String -> String -> Result Problem ( State, SideEffect )
init jsonFilePath json =
    case Json.decodeString PackageMetadata.decoder json of
        Ok metadata ->
            Ok ( (), NoOp )

        Err err ->
            Err <| InvalidPackageMetadata jsonFilePath (Json.errorToString err)
