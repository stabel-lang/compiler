module Play.PackageLoader exposing
    ( Model(..)
    , Msg(..)
    , Problem(..)
    , SideEffect(..)
    , getSideEffect
    , init
    , update
    )

import Dict exposing (Dict)
import Json.Decode as Json
import List.Extra as List
import Play.Data.Metadata as Metadata
import Play.Data.ModuleName as ModuleName exposing (ModuleName)
import Play.Data.PackageMetadata as PackageMetadata exposing (PackageMetadata)
import Play.Data.PackageName as PackageName exposing (PackageName)
import Play.Data.PackagePath as PackagePath exposing (PackagePath)
import Play.Data.SemanticVersion as SemanticVersion exposing (SemanticVersion)
import Play.Parser as Parser
import Play.Qualifier as Qualifier
import Result.Extra as Result
import Set exposing (Set)


type Problem
    = InvalidPackageMetadata String String
    | UnknownMessageForState String
    | NoExposedModulesInRootProject
    | ModuleNotFound String
    | InternalError String


type alias InitOptions =
    { projectDirPath : String
    , stdLibPath : String
    , possibleEntryPoint : Maybe String
    }


type Model
    = Initializing InitOptions SideEffect
    | LoadingMetadata State (List PackagePath) SideEffect
    | ResolvingModulePaths State (List PackageInfo) SideEffect
    | Parsing State (List ( PackageInfo, ModuleName )) SideEffect
    | Done Qualifier.ExposedAST
    | Failed Problem


type alias State =
    { possibleEntryPoint : Maybe String
    , rootPackage : PackageInfo
    , dependencies : Dict String SemanticVersion
    , dependentPackages : Dict String PackageInfo
    , filePathToModule : Dict String ( PackageName, ModuleName )
    , moduleNameToPackageName : Dict String String
    , absoluteModuleNameToDetails : Dict String ( PackageInfo, ModuleName )
    , parsedModuleNames : Set String
    , parsedModules : List ParsedModuleInfo
    }


type alias PackageInfo =
    { path : String
    , metadata : PackageMetadata
    , modules : List ModuleName
    }


type alias ParsedModuleInfo =
    { packageName : PackageName
    , modulePath : ModuleName
    , ast : Parser.AST
    }


emptyState : InitOptions -> PackageInfo -> State
emptyState initOptions rootPackage =
    { possibleEntryPoint = initOptions.possibleEntryPoint
    , rootPackage = rootPackage
    , dependencies = rootPackage.metadata.dependencies
    , dependentPackages = Dict.empty
    , filePathToModule = Dict.empty
    , moduleNameToPackageName = Dict.empty
    , absoluteModuleNameToDetails = Dict.empty
    , parsedModuleNames = Set.empty
    , parsedModules = []
    }


type Msg
    = FileContents String String String
    | ResolvedDirectories String (List PackagePath)
    | ResolvedPackageModules String (List String)


type SideEffect
    = ReadFile String String
    | ResolveDirectories String
    | ResolvePackageModules String String


getSideEffect : Model -> Maybe SideEffect
getSideEffect model =
    case model of
        Done _ ->
            Nothing

        Failed _ ->
            Nothing

        Initializing _ sf ->
            Just sf

        LoadingMetadata _ _ sf ->
            Just sf

        ResolvingModulePaths _ _ sf ->
            Just sf

        Parsing _ _ sf ->
            Just sf


init : InitOptions -> Model
init initOptions =
    Initializing initOptions <|
        ReadFile initOptions.projectDirPath "play.json"


update : Msg -> Model -> Model
update msg model =
    case model of
        Initializing initOpts _ ->
            case msg of
                FileContents path _ content ->
                    case Json.decodeString PackageMetadata.decoder content of
                        Ok metadata ->
                            let
                                metadataWithStdLib =
                                    { metadata
                                        | packagePaths =
                                            metadata.packagePaths ++ [ PackagePath.fromString initOpts.stdLibPath ]
                                    }

                                state =
                                    emptyState initOpts
                                        { path = path
                                        , metadata = metadataWithStdLib
                                        , modules = []
                                        }

                                pathsToLoad =
                                    List.map (PackagePath.prefix path) metadataWithStdLib.packagePaths
                            in
                            case pathsToLoad of
                                [] ->
                                    ResolvingModulePaths state [] <|
                                        ResolvePackageModules (PackageName.toString metadata.name) path

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

        Parsing state remainingModules _ ->
            parsingUpdate msg state remainingModules

        Done _ ->
            model

        Failed _ ->
            model


loadingMetadataUpdate : Msg -> State -> List PackagePath -> Model
loadingMetadataUpdate msg state remainingPaths =
    let
        nextStep pathsToLoad nextState pathPrefix =
            case pathsToLoad of
                [] ->
                    ResolvingModulePaths nextState
                        (Dict.values nextState.dependentPackages)
                        (ResolvePackageModules
                            (PackageName.toString nextState.rootPackage.metadata.name)
                            nextState.rootPackage.path
                        )

                (PackagePath.Directory nextPathDir) :: _ ->
                    LoadingMetadata nextState pathsToLoad <|
                        ReadFile nextPathDir "play.json"

                (PackagePath.AllDirectoriesInDirectory nextPathDir) :: _ ->
                    LoadingMetadata nextState pathsToLoad <|
                        ResolveDirectories (pathPrefix ++ nextPathDir)
    in
    case msg of
        FileContents path _ content ->
            case Json.decodeString PackageMetadata.decoder content of
                Ok metadata ->
                    case Dict.get (PackageName.toString metadata.name) state.dependencies of
                        Nothing ->
                            -- This package is not required, so ignore it
                            let
                                pathsToLoad =
                                    List.remove (PackagePath.Directory path) remainingPaths
                            in
                            nextStep pathsToLoad state ""

                        Just _ ->
                            let
                                updatedState =
                                    -- TODO: Register dependencies of sub-packages
                                    { state
                                        | dependentPackages =
                                            Dict.update
                                                (PackageName.toString metadata.name)
                                                (insertHighestPackage
                                                    { path = path
                                                    , metadata = metadata
                                                    , modules = []
                                                    }
                                                )
                                                state.dependentPackages
                                    }

                                absolutePackagePaths =
                                    List.map (PackagePath.prefix path) metadata.packagePaths

                                pathsToLoad =
                                    remainingPaths
                                        |> List.remove (PackagePath.Directory path)
                                        |> (++) absolutePackagePaths
                            in
                            nextStep pathsToLoad updatedState ""

                Err err ->
                    Debug.todo (Json.errorToString err)

        ResolvedDirectories parentDir paths ->
            let
                pathsToLoad =
                    remainingPaths
                        |> List.remove (PackagePath.AllDirectoriesInDirectory parentDir)
                        |> (++) paths
            in
            nextStep pathsToLoad state (parentDir ++ "/")

        _ ->
            Failed <| UnknownMessageForState "LoadingMetadata"


insertHighestPackage : PackageInfo -> Maybe PackageInfo -> Maybe PackageInfo
insertHighestPackage packageInfo maybeExistingPackage =
    case maybeExistingPackage of
        Nothing ->
            Just packageInfo

        Just existingPackage ->
            if
                SemanticVersion.compatible
                    existingPackage.metadata.version
                    packageInfo.metadata.version
                    == SemanticVersion.GreaterThanOrEqual
            then
                Just packageInfo

            else
                Just existingPackage


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
                            initCompileStep newState
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


initCompileStep : State -> Model
initCompileStep state =
    case state.rootPackage.metadata.exposedModules of
        [] ->
            Failed NoExposedModulesInRootProject

        firstExposedModule :: remModules ->
            if List.member firstExposedModule state.rootPackage.modules then
                let
                    ( path, fileName ) =
                        readModuleFromDisk state.rootPackage.path firstExposedModule

                    allPackages =
                        state.rootPackage :: Dict.values state.dependentPackages

                    pathsToModuleNames =
                        List.foldl pathsOfModules Dict.empty allPackages

                    moduleNameToPackageName =
                        List.foldl absolutePathsOfModules Dict.empty allPackages

                    absoluteNameToDetails =
                        allPackages
                            |> List.concatMap (\pInfo -> List.map (\m -> ( pInfo, m )) pInfo.modules)
                            |> List.map (\( pInfo, mName ) -> ( absoluteModuleName pInfo.metadata.name mName, ( pInfo, mName ) ))
                            |> Dict.fromList
                in
                Parsing
                    { state
                        | filePathToModule = pathsToModuleNames
                        , moduleNameToPackageName = moduleNameToPackageName
                        , absoluteModuleNameToDetails = absoluteNameToDetails
                    }
                    (List.map (\m -> ( state.rootPackage, m )) remModules)
                    (ReadFile path fileName)

            else
                Failed (ModuleNotFound (ModuleName.toString firstExposedModule))


readModuleFromDisk : String -> ModuleName -> ( String, String )
readModuleFromDisk packagePath moduleName =
    let
        ( path, fileName ) =
            case List.reverse (ModuleName.toPartStrings moduleName) of
                name :: reversePath ->
                    ( reversePath
                        |> List.reverse
                        |> String.join "/"
                    , name ++ ".play"
                    )

                _ ->
                    ( "", "" )
    in
    ( [ packagePath, "src", path ]
        |> List.filter (not << String.isEmpty)
        |> String.join "/"
    , fileName
    )


pathsOfModules : PackageInfo -> Dict String ( PackageName, ModuleName ) -> Dict String ( PackageName, ModuleName )
pathsOfModules package acc =
    let
        modulePaths =
            List.map (\moduleName -> ( pathToModule moduleName, ( package.metadata.name, moduleName ) )) package.modules
                |> Dict.fromList

        pathToModule moduleName =
            let
                ( path, fileName ) =
                    readModuleFromDisk package.path moduleName
            in
            path ++ "/" ++ fileName
    in
    Dict.union acc modulePaths


absolutePathsOfModules : PackageInfo -> Dict String String -> Dict String String
absolutePathsOfModules package acc =
    let
        absolutePathsForModule =
            List.map
                (\moduleName ->
                    ( "/" ++ ModuleName.toString moduleName
                    , PackageName.toString package.metadata.name
                    )
                )
                package.modules
                |> Dict.fromList
    in
    Dict.union acc absolutePathsForModule


parsingUpdate : Msg -> State -> List ( PackageInfo, ModuleName ) -> Model
parsingUpdate msg state remainingModules =
    case msg of
        FileContents path fileName content ->
            let
                fullPath =
                    path ++ "/" ++ fileName

                possibleModuleInfo =
                    Dict.get fullPath state.filePathToModule
            in
            case ( possibleModuleInfo, Parser.run content ) of
                ( _, Err parserError ) ->
                    Failed <| InternalError <| "Parser error: " ++ Debug.toString parserError

                ( Just ( packageName, moduleName ), Ok parserAst ) ->
                    let
                        fullModuleName =
                            absoluteModuleName packageName moduleName

                        updatedParsedModules =
                            Set.insert fullModuleName state.parsedModuleNames

                        updatedState =
                            { state
                                | parsedModuleNames = updatedParsedModules
                                , parsedModules =
                                    { packageName = packageName
                                    , modulePath = moduleName
                                    , ast = parserAst
                                    }
                                        :: state.parsedModules
                            }

                        requiredModules =
                            Qualifier.requiredModules
                                { packageName = PackageName.toString packageName
                                , modulePath = ModuleName.toString moduleName
                                , ast = parserAst
                                , externalModules = state.moduleNameToPackageName
                                }

                        modulesQueuedForOrAlreadyParsed =
                            remainingModules
                                |> List.map (\( pInfo, mName ) -> absoluteModuleName pInfo.metadata.name mName)
                                |> Set.fromList
                                |> Set.union updatedParsedModules

                        missingModulesInParseQueue =
                            Set.diff requiredModules modulesQueuedForOrAlreadyParsed
                                |> Set.toList
                                |> List.filterMap (\absName -> Dict.get absName state.absoluteModuleNameToDetails)

                        updatedRemainingModules =
                            remainingModules ++ missingModulesInParseQueue
                    in
                    nextCompileStep updatedRemainingModules updatedState

                ( Nothing, _ ) ->
                    Failed <| InternalError <| "Don't know why we read file: " ++ fullPath

        _ ->
            Failed <| InternalError <| "Unknown message for compile stage: " ++ Debug.toString msg


absoluteModuleName : PackageName -> ModuleName -> String
absoluteModuleName packageName moduleName =
    "/" ++ PackageName.toString packageName ++ "/" ++ ModuleName.toString moduleName


nextCompileStep : List ( PackageInfo, ModuleName ) -> State -> Model
nextCompileStep remainingModules state =
    case remainingModules of
        [] ->
            let
                ( qualifiedAst, errs ) =
                    state.parsedModules
                        |> List.foldl qualifyAst ( { types = Dict.empty, words = Dict.empty }, [] )

                qualifyAst parsedModInfo ( qast, es ) =
                    let
                        qualifierResult =
                            Qualifier.run
                                { packageName = PackageName.toString parsedModInfo.packageName
                                , modulePath = ModuleName.toString parsedModInfo.modulePath
                                , ast = parsedModInfo.ast
                                , externalModules = state.moduleNameToPackageName
                                }
                    in
                    case qualifierResult of
                        Err qualifierError ->
                            ( qast
                            , (InternalError <| "Qualifier error: " ++ Debug.toString qualifierError) :: es
                            )

                        Ok qualifiedAST ->
                            let
                                mergedQualifiedAst =
                                    { types = Dict.union qast.types qualifiedAST.types
                                    , words = Dict.union qast.words qualifiedAST.words
                                    }
                            in
                            ( mergedQualifiedAst, es )

                wordsWithEntryPoint =
                    state.possibleEntryPoint
                        |> Maybe.map (setEntryPoint qualifiedAst.words)
                        |> Maybe.withDefault qualifiedAst.words

                setEntryPoint words entryPointName =
                    Dict.update
                        entryPointName
                        (Maybe.map (\w -> { w | metadata = Metadata.asEntryPoint w.metadata }))
                        words
            in
            case errs of
                [] ->
                    Done
                        { types = qualifiedAst.types
                        , words = wordsWithEntryPoint
                        }

                _ ->
                    Failed <| InternalError <| "Qualification failed with error: " ++ Debug.toString errs

        ( packageInfo, moduleName ) :: otherModules ->
            let
                ( path, fileName ) =
                    readModuleFromDisk packageInfo.path moduleName
            in
            Parsing state otherModules (ReadFile path fileName)
