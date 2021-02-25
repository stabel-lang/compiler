module Play.PackageLoader exposing
    ( Model(..)
    , Msg(..)
    , Problem(..)
    , SideEffect(..)
    , init
    , update
    )

import Dict exposing (Dict)
import Json.Decode as Json
import List.Extra as List
import Play.Data.PackageMetadata as PackageMetadata exposing (PackageMetadata)
import Play.Data.PackageName as PackageName
import Play.Data.PackagePath as PackagePath exposing (PackagePath)


type Problem
    = InvalidPackageMetadata String String


type Model
    = Initializing SideEffect
    | LoadingMetadata State (List PackagePath) SideEffect
    | Done State
    | Failed Problem


type alias State =
    { rootPackage : PackageInfo
    , dependentPackages : Dict String PackageInfo
    }


type alias PackageInfo =
    { path : String
    , metadata : PackageMetadata
    }


emptyState : PackageInfo -> State
emptyState rootPackage =
    { rootPackage = rootPackage
    , dependentPackages = Dict.empty
    }


type Msg
    = FileContents String String String
    | ResolvedDirectories String (List PackagePath)


type SideEffect
    = ReadFile String String
    | ResolveDirectories String


init : String -> Model
init projectDirPath =
    Initializing
        (ReadFile projectDirPath "play.json")


update : Msg -> Model -> Model
update msg model =
    case model of
        Initializing _ ->
            case msg of
                FileContents path _ content ->
                    case Json.decodeString PackageMetadata.decoder content of
                        Ok metadata ->
                            let
                                state =
                                    emptyState
                                        { path = path
                                        , metadata = metadata
                                        }

                                pathsToLoad =
                                    List.map (PackagePath.prefix path) metadata.packagePaths
                            in
                            case pathsToLoad of
                                [] ->
                                    Done state

                                (PackagePath.Directory nextPathDir) :: _ ->
                                    LoadingMetadata state pathsToLoad <|
                                        ReadFile nextPathDir "play.json"

                                (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                                    LoadingMetadata state pathsToLoad <|
                                        ResolveDirectories nextPathDir

                        Err err ->
                            Failed <| InvalidPackageMetadata path <| Json.errorToString err

                _ ->
                    Failed (InvalidPackageMetadata "todo: path" "Wrong message on initialization")

        LoadingMetadata state remainingPaths _ ->
            loadingMetadataUpdate msg state remainingPaths

        Done _ ->
            model

        Failed _ ->
            model


loadingMetadataUpdate : Msg -> State -> List PackagePath -> Model
loadingMetadataUpdate msg state remainingPaths =
    case msg of
        FileContents path _ content ->
            case Json.decodeString PackageMetadata.decoder content of
                Ok metadata ->
                    let
                        updatedState =
                            { state
                                | dependentPackages =
                                    Dict.insert
                                        (PackageName.toString metadata.name)
                                        { path = path
                                        , metadata = metadata
                                        }
                                        state.dependentPackages
                            }

                        absolutePackagePaths =
                            List.map (PackagePath.prefix path) metadata.packagePaths

                        pathsToLoad =
                            remainingPaths
                                |> List.remove (PackagePath.Directory path)
                                |> (++) absolutePackagePaths
                    in
                    case pathsToLoad of
                        [] ->
                            Done updatedState

                        (PackagePath.Directory nextPathDir) :: _ ->
                            LoadingMetadata updatedState pathsToLoad <|
                                ReadFile nextPathDir "play.json"

                        (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                            LoadingMetadata updatedState pathsToLoad <|
                                ResolveDirectories nextPathDir

                Err err ->
                    Debug.todo (Json.errorToString err)

        ResolvedDirectories parentDir paths ->
            let
                pathsToLoad =
                    remainingPaths
                        |> List.remove (PackagePath.AllDirectoriesInDirectory parentDir)
                        |> (++) paths
            in
            case pathsToLoad of
                [] ->
                    Done state

                (PackagePath.Directory nextPathDir) :: _ ->
                    LoadingMetadata state pathsToLoad <|
                        ReadFile nextPathDir "play.json"

                (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                    LoadingMetadata state pathsToLoad <|
                        ResolveDirectories (parentDir ++ "/" ++ nextPathDir)
