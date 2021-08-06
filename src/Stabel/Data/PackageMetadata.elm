module Stabel.Data.PackageMetadata exposing
    ( PackageMetadata
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as Json
import Stabel.Data.ModuleName as ModuleName exposing (ModuleName)
import Stabel.Data.PackageName as PackageName exposing (PackageName)
import Stabel.Data.PackagePath as PackagePath exposing (PackagePath)
import Stabel.Data.SemanticVersion as SemanticVersion exposing (SemanticVersion)


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
        (Json.field "name" packageNameDecoder)
        (Json.field "version" semverDecoder)
        (Json.field "language-version" semverDecoder)
        (Json.field "exposed-modules" (Json.list moduleNameDecoder))
        (Json.field "dependencies" (Json.dict semverDecoder |> Json.andThen validateDependencyKeys))
        (Json.field "package-paths" (Json.list packagePathDecoder))


packageNameDecoder : Json.Decoder PackageName
packageNameDecoder =
    Json.string
        |> Json.andThen (resultDecodeAdapt << PackageName.fromString)


semverDecoder : Json.Decoder SemanticVersion
semverDecoder =
    Json.string
        |> Json.andThen (resultDecodeAdapt << SemanticVersion.fromString)


moduleNameDecoder : Json.Decoder ModuleName
moduleNameDecoder =
    Json.string
        |> Json.andThen (resultDecodeAdapt << ModuleName.fromString)


packagePathDecoder : Json.Decoder PackagePath
packagePathDecoder =
    Json.string
        |> Json.map PackagePath.fromString


validateDependencyKeys : Dict String SemanticVersion -> Json.Decoder (Dict String SemanticVersion)
validateDependencyKeys deps =
    let
        isValid =
            Dict.keys deps
                |> List.all (\k -> Result.toMaybe (PackageName.fromString k) /= Nothing)
    in
    if isValid then
        Json.succeed deps

    else
        Json.fail "Invalid dependency package name"


resultDecodeAdapt : Result err ok -> Json.Decoder ok
resultDecodeAdapt result =
    case result of
        Ok value ->
            Json.succeed value

        Err _ ->
            Json.fail "Something went wrong"
