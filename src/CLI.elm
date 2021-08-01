port module CLI exposing (main)

import Json.Decode as Json
import Json.Encode as Encode
import Platform exposing (Program)
import Set
import Stabel.Codegen as Codegen
import Stabel.Data.PackagePath as PackagePath
import Stabel.PackageLoader as PackageLoader
import Stabel.TypeChecker as TypeChecker
import Stabel.TypeChecker.Problem as TypeCheckerProblem
import Stabel.Wasm as Wasm


type alias Flags =
    { projectDir : String
    , entryPoint : Maybe String
    , stdLibPath : String
    }


type alias Model =
    ( Maybe String, PackageLoader.Model )


type Msg
    = Incomming Json.Value


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
    ( ( entryPoint, initialModel )
    , sendSideEffectFromModel initialModel
    )


sendSideEffectFromModel : PackageLoader.Model -> Cmd Msg
sendSideEffectFromModel model =
    PackageLoader.getSideEffect model
        |> Maybe.map encodeSideEffectAsJson
        |> Maybe.map outgoingPort
        |> Maybe.withDefault Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (( entryPoint, packageLoaderModel ) as model) =
    case msg of
        Incomming packageLoaderMsgJson ->
            case Json.decodeValue decodePackageLoaderMsg packageLoaderMsgJson of
                Ok packageLoaderMsg ->
                    let
                        updatedModel =
                            PackageLoader.update packageLoaderMsg packageLoaderModel
                    in
                    case updatedModel of
                        PackageLoader.Done qualifiedAst ->
                            let
                                compilationResult =
                                    case TypeChecker.run qualifiedAst of
                                        Err typeErrors ->
                                            formatErrors (TypeCheckerProblem.toString "") typeErrors

                                        Ok typedAst ->
                                            let
                                                exportedFunctions =
                                                    entryPoint
                                                        |> Maybe.map Set.singleton
                                                        |> Maybe.withDefault Set.empty
                                            in
                                            typedAst
                                                |> Codegen.run exportedFunctions
                                                |> Wasm.toString
                                                |> Ok
                            in
                            case compilationResult of
                                Ok wast ->
                                    ( model
                                    , outgoingPort <| encodeCompilationDone wast
                                    )

                                Err error ->
                                    ( model
                                    , outgoingPort <| encodeCompilationFailure error
                                    )

                        PackageLoader.Failed error ->
                            ( model
                            , outgoingPort <|
                                encodeCompilationFailure <|
                                    PackageLoader.problemToString error
                            )

                        _ ->
                            ( ( entryPoint, updatedModel )
                            , sendSideEffectFromModel updatedModel
                            )

                Err decodeError ->
                    ( model
                    , outgoingPort <|
                        encodeCompilationFailure <|
                            Json.errorToString decodeError
                    )


formatErrors : (a -> String) -> List a -> Result String b
formatErrors fn problems =
    problems
        |> List.map fn
        |> String.join "\n\n"
        |> Err



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


encodeCompilationDone : String -> Json.Value
encodeCompilationDone wast =
    Encode.object
        [ ( "type", Encode.string "compilationDone" )
        , ( "wast", Encode.string wast )
        ]


encodeCompilationFailure : String -> Json.Value
encodeCompilationFailure errorMsg =
    Encode.object
        [ ( "type", Encode.string "compilationFailure" )
        , ( "error", Encode.string errorMsg )
        ]


decodePackageLoaderMsg : Json.Decoder PackageLoader.Msg
decodePackageLoaderMsg =
    let
        helper typeStr =
            case typeStr of
                "fileContents" ->
                    Json.map3 PackageLoader.FileContents
                        (Json.field "path" Json.string)
                        (Json.field "fileName" Json.string)
                        (Json.field "content" Json.string)

                "resolvedPackageModules" ->
                    Json.map2 PackageLoader.ResolvedPackageModules
                        (Json.field "package" Json.string)
                        (Json.field "modules" (Json.list Json.string))

                "resolvedDirectories" ->
                    Json.map2 PackageLoader.ResolvedDirectories
                        (Json.field "parentDir" Json.string)
                        (Json.field "paths" (Json.list packagePathDecoder))

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
