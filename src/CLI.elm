port module CLI exposing (main)

import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode
import Platform exposing (Program)
import Set
import Stabel.Codegen as Codegen
import Stabel.Data.PackagePath as PackagePath
import Stabel.Data.SourceLocation as SourceLocation
import Stabel.Data.Type as Type
import Stabel.PackageLoader as PackageLoader
import Stabel.Qualifier as Qualifier
import Stabel.TypeChecker as TypeChecker
import Stabel.TypeChecker.Problem as TypeCheckerProblem
import Stabel.Wasm as Wasm


type alias Flags =
    { projectDir : String
    , entryPoint : Maybe String
    , stdLibPath : String
    }


type alias Model =
    -- TODO: Do better
    ( Maybe String
    , PackageLoader.Model
    , List TypeCheckerProblem.Problem
    )


type Msg
    = Incomming Json.Value


type CliMsg
    = FilesForErrorReporting (Dict String String)
    | PackageLoaderMsg PackageLoader.Msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { projectDir, entryPoint, stdLibPath } =
    let
        initialModel =
            PackageLoader.init
                { projectDirPath = projectDir
                , stdLibPath = stdLibPath
                }
    in
    ( ( entryPoint, initialModel, [] )
    , sendSideEffectFromModel initialModel
    )


sendSideEffectFromModel : PackageLoader.Model -> Cmd Msg
sendSideEffectFromModel model =
    PackageLoader.getSideEffect model
        |> Maybe.map encodeSideEffectAsJson
        |> Maybe.map outgoingPort
        |> Maybe.withDefault Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incomming packageLoaderMsgJson ->
            case Json.decodeValue decodePackageLoaderMsg packageLoaderMsgJson of
                Ok cliMsg ->
                    cliUpdate cliMsg model

                Err decodeError ->
                    ( model
                    , outgoingPort <|
                        encodeCompilationFailure <|
                            Json.errorToString decodeError
                    )


cliUpdate : CliMsg -> Model -> ( Model, Cmd Msg )
cliUpdate msg (( entryPoint, packageLoaderModel, typeErrors_ ) as model) =
    case msg of
        PackageLoaderMsg packageLoaderMsg ->
            let
                updatedModel =
                    PackageLoader.update packageLoaderMsg packageLoaderModel
            in
            case updatedModel of
                PackageLoader.Done qualifiedAst ->
                    case typeCheckAndRun entryPoint qualifiedAst of
                        Ok ( wast, isStringReturn ) ->
                            ( ( entryPoint, updatedModel, typeErrors_ )
                            , outgoingPort <| encodeCompilationDone wast isStringReturn
                            )

                        Err ( sourceFilesRequired, errors ) ->
                            ( ( entryPoint, updatedModel, errors )
                            , outgoingPort <| encodeReadFilesToReportError sourceFilesRequired
                            )

                PackageLoader.Failed error ->
                    ( ( entryPoint, updatedModel, typeErrors_ )
                    , outgoingPort <|
                        encodeCompilationFailure <|
                            PackageLoader.problemToString error
                    )

                _ ->
                    ( ( entryPoint, updatedModel, typeErrors_ )
                    , sendSideEffectFromModel updatedModel
                    )

        FilesForErrorReporting files ->
            let
                errorMessages =
                    typeErrors_
                        |> List.map
                            (\problem ->
                                files
                                    |> Dict.get (TypeCheckerProblem.sourceLocationRef problem)
                                    |> Maybe.withDefault ""
                                    |> (\source -> TypeCheckerProblem.toString source problem)
                            )
                        |> String.join "\n\n"
            in
            ( model
            , outgoingPort <|
                encodeCompilationFailure errorMessages
            )


legalIntEntryPointType : Type.FunctionType
legalIntEntryPointType =
    { input = []
    , output = [ Type.Int ]
    }


legalStringEntryPointType : Type.FunctionType
legalStringEntryPointType =
    { input = []
    , output = [ Type.Custom "/stabel/standard_library/string/String" ]
    }


typeCheckAndRun :
    Maybe String
    -> Qualifier.AST
    -> Result ( List String, List TypeCheckerProblem.Problem ) ( String, Bool )
typeCheckAndRun entryPoint qualifiedAst =
    case TypeChecker.run qualifiedAst of
        Err typeErrors ->
            let
                sourceFiles =
                    typeErrors
                        |> List.map TypeCheckerProblem.sourceLocationRef
                        |> Set.fromList
                        |> Set.toList
            in
            Err ( sourceFiles, typeErrors )

        Ok typedAst ->
            let
                exportedFunctions =
                    entryPoint
                        |> Maybe.map Set.singleton
                        |> Maybe.withDefault Set.empty

                entryPointFunction =
                    entryPoint
                        |> Maybe.andThen (\n -> Dict.get n typedAst.functions)
            in
            case entryPointFunction of
                Just fn ->
                    if fn.type_ == legalIntEntryPointType then
                        typedAst
                            |> Codegen.run exportedFunctions
                            |> Wasm.toString
                            |> (\wat -> Ok ( wat, False ))

                    else if fn.type_ == legalStringEntryPointType then
                        typedAst
                            |> Codegen.run exportedFunctions
                            |> Wasm.toString
                            |> (\wat -> Ok ( wat, True ))

                    else
                        let
                            sourceLoc =
                                fn.sourceLocation
                                    |> Maybe.withDefault SourceLocation.emptyRange
                        in
                        Err
                            ( [ sourceLoc.source ]
                            , [ TypeCheckerProblem.BadEntryPoint
                                    sourceLoc
                                    fn.name
                                    legalIntEntryPointType
                                    fn.type_
                              ]
                            )

                Nothing ->
                    typedAst
                        |> Codegen.run exportedFunctions
                        |> Wasm.toString
                        |> (\wat -> Ok ( wat, False ))



-- Json Encoding/Decoding


encodeSideEffectAsJson : PackageLoader.SideEffect -> Json.Value
encodeSideEffectAsJson sf =
    case sf of
        PackageLoader.ReadFile path fileName ->
            Encode.object
                [ ( "type", Encode.string "readFile" )
                , ( "path", Encode.string path )
                , ( "fileName", Encode.string fileName )
                ]

        PackageLoader.ResolveDirectories path ->
            Encode.object
                [ ( "type", Encode.string "resolveDirectories" )
                , ( "path", Encode.string path )
                ]

        PackageLoader.ResolvePackageModules moduleName path ->
            Encode.object
                [ ( "type", Encode.string "resolvePackageModules" )
                , ( "package", Encode.string moduleName )
                , ( "path", Encode.string path )
                ]


encodeCompilationDone : String -> Bool -> Json.Value
encodeCompilationDone wast isStringReturn =
    Encode.object
        [ ( "type", Encode.string "compilationDone" )
        , ( "wast", Encode.string wast )
        , ( "isStringReturned", Encode.bool isStringReturn )
        ]


encodeCompilationFailure : String -> Json.Value
encodeCompilationFailure errorMsg =
    Encode.object
        [ ( "type", Encode.string "compilationFailure" )
        , ( "error", Encode.string ("\n" ++ errorMsg ++ "\n") )
        ]


encodeReadFilesToReportError : List String -> Json.Value
encodeReadFilesToReportError files =
    Encode.object
        [ ( "type", Encode.string "readFilesToReportError" )
        , ( "paths", Encode.list Encode.string files )
        ]


decodePackageLoaderMsg : Json.Decoder CliMsg
decodePackageLoaderMsg =
    let
        helper typeStr =
            case typeStr of
                "fileContents" ->
                    Json.map PackageLoaderMsg <|
                        Json.map3 PackageLoader.FileContents
                            (Json.field "path" Json.string)
                            (Json.field "fileName" Json.string)
                            (Json.field "content" Json.string)

                "resolvedPackageModules" ->
                    Json.map PackageLoaderMsg <|
                        Json.map2 PackageLoader.ResolvedPackageModules
                            (Json.field "package" Json.string)
                            (Json.field "modules" (Json.list Json.string))

                "resolvedDirectories" ->
                    Json.map PackageLoaderMsg <|
                        Json.map2 PackageLoader.ResolvedDirectories
                            (Json.field "parentDir" Json.string)
                            (Json.field "paths" (Json.list packagePathDecoder))

                "filesForErrorReporting" ->
                    Json.map FilesForErrorReporting
                        (Json.field "files" (Json.dict Json.string))

                _ ->
                    Json.fail <| "Unknown msg type: " ++ typeStr

        packagePathDecoder =
            Json.map PackagePath.fromString Json.string
    in
    Json.field "type" Json.string
        |> Json.andThen helper



-- PORTS


subscriptions : Model -> Sub Msg
subscriptions _ =
    incomingPort Incomming


port incomingPort : (Json.Value -> msg) -> Sub msg


port outgoingPort : Json.Value -> Cmd msg
