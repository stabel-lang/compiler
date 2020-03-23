port module Main exposing (main)

import Platform exposing (Program)
import Play.Codegen as Codegen
import Play.Parser as Parser
import Play.Qualifier as Qualifier
import Play.Tokenizer as Tokenizer
import Wasm


type alias Model =
    ()


type Msg
    = CompileString String


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompileString sourceCode ->
            case compile sourceCode of
                Ok wasm ->
                    ( ()
                    , compileFinished ( True, Wasm.toString wasm )
                    )

                Err () ->
                    ( ()
                    , compileFinished ( False, "Compilation failed" )
                    )


compile : String -> Result () Wasm.Module
compile sourceCode =
    sourceCode
        |> Tokenizer.tokenize
        |> Result.andThen Parser.parse
        |> Result.andThen Qualifier.qualify
        |> Result.andThen Codegen.codegen


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ compileString CompileString ]


port compileString : (String -> msg) -> Sub msg


port compileFinished : ( Bool, String ) -> Cmd msg
