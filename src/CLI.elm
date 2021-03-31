port module CLI exposing (main)

import Json.Decode as Json
import Json.Encode as Encode
import Platform exposing (Program)
import Play.Codegen as Codegen
import Play.PackageLoader as PackageLoader
import Play.Parser as Parser
import Play.Parser.Problem as ParserProblem
import Play.Qualifier as Qualifier
import Play.Qualifier.Problem as QualifierProblem
import Play.TypeChecker as TypeChecker
import Play.TypeChecker.Problem as TypeCheckerProblem
import Wasm


type alias Model =
    PackageLoader.Model


type Msg
    = Incomming Json.Value


main : Program String Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : String -> ( Model, Cmd Msg )
init projectDir =
    let
        initialModel =
            PackageLoader.init projectDir
    in
    ( initialModel
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
                Ok packageLoaderMsg ->
                    let
                        updatedModel =
                            PackageLoader.update packageLoaderMsg model
                                |> Debug.log "new model"
                    in
                    ( updatedModel
                    , sendSideEffectFromModel updatedModel
                    )

                Err decodeError ->
                    let
                        _ =
                            Debug.log "error" decodeError
                    in
                    ( model
                    , Cmd.none
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
                , ( "module", Encode.string moduleName )
                , ( "path", Encode.string path )
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

                _ ->
                    Json.fail <| "Unknown msg type: " ++ typeStr
    in
    Json.field "type" Json.string
        |> Json.andThen helper



-- PORTS


subscriptions : Model -> Sub Msg
subscriptions _ =
    incomingPort Incomming


port incomingPort : (Json.Value -> msg) -> Sub msg


port outgoingPort : Json.Value -> Cmd msg
