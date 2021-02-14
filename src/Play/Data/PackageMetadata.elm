module Play.Data.PackageMetadata exposing
    ( PackageMetadata
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as Json
import Play.Data.ModuleName as ModuleName exposing (ModuleName)
import Play.Data.PackageName as PackageName exposing (PackageName)
import Play.Data.PackagePath as PackagePath exposing (PackagePath)
import Play.Data.SemanticVersion as SemanticVersion exposing (SemanticVersion)


type alias PackageMetadata =
    { name : PackageName
    , version : SemanticVersion
    , compatibleLanguageVersion : SemanticVersion
    , exposedModules : List ModuleName
    , dependencies : Dict String SemanticVersion
    , packagePaths : List PackagePath
    }


decoder : Json.Decoder PackageMetadata
decoder =
    Json.map6 PackageMetadata
        (Json.field "name" Json.string |> Json.andThen (resultDecodeAdapt << PackageName.fromString))
        (Json.field "version" Json.string |> Json.andThen (resultDecodeAdapt << SemanticVersion.fromString))
        (Json.field "language-version" Json.string |> Json.andThen (resultDecodeAdapt << SemanticVersion.fromString))
        (Json.field "exposed-modules" (Json.list (Json.string |> Json.andThen (resultDecodeAdapt << ModuleName.fromString))))
        (Json.field "dependencies" (Json.dict (Json.string |> Json.andThen (resultDecodeAdapt << SemanticVersion.fromString))))
        (Json.field "package-paths" (Json.list (Json.string |> Json.map PackagePath.fromString)))


resultDecodeAdapt : Result err ok -> Json.Decoder ok
resultDecodeAdapt result =
    case result of
        Ok value ->
            Json.succeed value

        Err err ->
            Json.fail "Something went wrong"
