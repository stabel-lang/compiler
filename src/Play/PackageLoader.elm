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
import Play.Data.ModuleName as ModuleName exposing (ModuleName)
import Play.Data.PackageMetadata as PackageMetadata exposing (PackageMetadata)
import Play.Data.PackageName as PackageName
import Play.Data.PackagePath as PackagePath exposing (PackagePath)
import Result.Extra as Result


type Problem
    = InvalidPackageMetadata String String
    | UnknownMessageForState String
    | InternalError String


type Model
    = Initializing SideEffect
    | LoadingMetadata State (List PackagePath) SideEffect
    | ResolvingModulePaths State (List PackageInfo) SideEffect
    | Done State
    | Failed Problem


type alias State =
    { rootPackage : PackageInfo
    , dependentPackages : Dict String PackageInfo
    }


type alias PackageInfo =
    { path : String
    , metadata : PackageMetadata
    , modules : List ModuleName
    }


emptyState : PackageInfo -> State
emptyState rootPackage =
    { rootPackage = rootPackage
    , dependentPackages = Dict.empty
    }


type Msg
    = FileContents String String String
    | ResolvedDirectories String (List PackagePath)
    | ResolvedPackageModules String (List String)


type SideEffect
    = ReadFile String String
    | ResolveDirectories String
    | ResolvePackageModules String String


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
                                        , modules = []
                                        }

                                pathsToLoad =
                                    List.map (PackagePath.prefix path) metadata.packagePaths
                            in
                            case pathsToLoad of
                                [] ->
                                    ResolvingModulePaths state [] (ResolvePackageModules (PackageName.toString metadata.name) path)

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

        ResolvingModulePaths state remainingPackages _ ->
            resolvingModulePathsUpdate msg state remainingPackages

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
                                        , modules = []
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
                            ResolvingModulePaths updatedState
                                (Dict.values updatedState.dependentPackages)
                                (ResolvePackageModules (PackageName.toString state.rootPackage.metadata.name) state.rootPackage.path)

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
                    ResolvingModulePaths state
                        (Dict.values state.dependentPackages)
                        (ResolvePackageModules (PackageName.toString state.rootPackage.metadata.name) state.rootPackage.path)

                (PackagePath.Directory nextPathDir) :: _ ->
                    LoadingMetadata state pathsToLoad <|
                        ReadFile nextPathDir "play.json"

                (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                    LoadingMetadata state pathsToLoad <|
                        ResolveDirectories (parentDir ++ "/" ++ nextPathDir)

        _ ->
            Failed <| UnknownMessageForState "LoadingMetadata"


resolvingModulePathsUpdate : Msg -> State -> List PackageInfo -> Model
resolvingModulePathsUpdate msg state remainingPackages =
    case msg of
        ResolvedPackageModules packageName modules ->
            let
                rootPackage =
                    state.rootPackage

                moduleNameResults =
                    List.map (String.replace ".play" "") modules
                        |> List.map ModuleName.fromString
                        |> Result.combine

                updatedRemainingPackages =
                    List.filter (\p -> PackageName.toString p.metadata.name /= packageName) remainingPackages

                nextStep newState =
                    case updatedRemainingPackages of
                        nextPackage :: _ ->
                            ResolvingModulePaths newState
                                updatedRemainingPackages
                                (ResolvePackageModules (PackageName.toString nextPackage.metadata.name) nextPackage.path)

                        [] ->
                            Done newState
            in
            case moduleNameResults of
                Err _ ->
                    Failed <| InternalError <| "Invalid module names for package " ++ packageName

                Ok moduleNames ->
                    if packageName == PackageName.toString state.rootPackage.metadata.name then
                        nextStep { state | rootPackage = { rootPackage | modules = moduleNames } }

                    else
                        case Dict.get packageName state.dependentPackages of
                            Nothing ->
                                Failed <| InternalError <| "Package " ++ packageName ++ " doesn't exist"

                            Just package ->
                                nextStep
                                    { state
                                        | dependentPackages =
                                            Dict.insert packageName { package | modules = moduleNames } state.dependentPackages
                                    }

        _ ->
            Failed <| UnknownMessageForState "ResolvingModulePaths"
