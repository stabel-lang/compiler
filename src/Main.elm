port module Main exposing (main)

import Platform exposing (Program)
import Play.Codegen as Codegen
import Play.Parser as Parser
import Play.Qualifier as Qualifier
import Play.TypeChecker as TypeChecker
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
update msg _ =
    case msg of
        CompileString sourceCode ->
            case compile sourceCode of
                Ok wasm ->
                    ( ()
                    , compileFinished ( True, Wasm.toString wasm )
                    )

                Err errmsg ->
                    ( ()
                    , compileFinished ( False, "Compilation failed:\n" ++ errmsg )
                    )


compile : String -> Result String Wasm.Module
compile sourceCode =
    case Parser.run sourceCode of
        Err parserError ->
            Err <| Debug.toString parserError

        Ok ast ->
            Qualifier.qualify ast
                |> Result.andThen TypeChecker.typeCheck
                |> Result.andThen Codegen.codegen
                |> Result.mapError Debug.toString


subscriptions : Model -> Sub Msg
subscriptions _ =
    compileString CompileString


port compileString : (String -> msg) -> Sub msg


port compileFinished : ( Bool, String ) -> Cmd msg
