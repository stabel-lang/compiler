module Play.PackageLoader exposing
    ( Model
    , Msg(..)
    , Problem
    , SideEffect(..)
    , doneState
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
    = LoadingMetadata State (List PackagePath)
    | Done State


doneState : Model -> Maybe State
doneState model =
    case model of
        Done state ->
            Just state

        _ ->
            Nothing


type alias State =
    { loadedPackages : Dict String PackageMetadata
    }


emptyState : State
emptyState =
    { loadedPackages = Dict.empty }


type Msg
    = FileContents String String String
    | ResolvedDirectories String (List PackagePath)


type SideEffect
    = NoOp
    | ReadFile String String
    | ResolveDirectories String


init : String -> ( Model, SideEffect )
init projectDirPath =
    ( LoadingMetadata emptyState [ PackagePath.Directory projectDirPath ]
    , ReadFile projectDirPath "play.json"
    )


update : Msg -> Model -> ( Model, SideEffect )
update msg model =
    case model of
        Done _ ->
            ( model, NoOp )

        LoadingMetadata state remainingPaths ->
            loadingMetadataUpdate msg state remainingPaths


loadingMetadataUpdate : Msg -> State -> List PackagePath -> ( Model, SideEffect )
loadingMetadataUpdate msg state remainingPaths =
    case msg of
        FileContents path _ content ->
            case Json.decodeString PackageMetadata.decoder content of
                Ok metadata ->
                    let
                        updatedState =
                            { state
                                | loadedPackages =
                                    Dict.insert
                                        (PackageName.toString metadata.name)
                                        metadata
                                        state.loadedPackages
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
                            ( Done updatedState, NoOp )

                        (PackagePath.Directory nextPathDir) :: _ ->
                            ( LoadingMetadata updatedState pathsToLoad
                            , ReadFile nextPathDir "play.json"
                            )

                        (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                            ( LoadingMetadata updatedState pathsToLoad
                            , ResolveDirectories nextPathDir
                            )

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
                    ( Done state, NoOp )

                (PackagePath.Directory nextPathDir) :: _ ->
                    ( LoadingMetadata state pathsToLoad
                    , ReadFile nextPathDir "play.json"
                    )

                (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                    ( LoadingMetadata state pathsToLoad
                    , ResolveDirectories <| parentDir ++ "/" ++ nextPathDir
                    )
