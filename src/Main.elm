port module Main exposing (main)

import Dict
import Platform exposing (Program)
import Play.Codegen as Codegen
import Play.Parser as Parser
import Play.Parser.Problem as ParserProblem
import Play.Qualifier as Qualifier
import Play.Qualifier.Problem as QualifierProblem
import Play.TypeChecker as TypeChecker
import Play.TypeChecker.Problem as TypeCheckerProblem
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
                    , compileFinished ( False, "Compilation failed:\n\n" ++ errmsg )
                    )


compile : String -> Result String Wasm.Module
compile sourceCode =
    case Parser.run sourceCode of
        Err parserErrors ->
            formatErrors (ParserProblem.toString sourceCode) parserErrors

        Ok ast ->
            let
                qualifierResult =
                    Qualifier.run
                        { packageName = ""
                        , modulePath = ""
                        , ast = ast
                        , externalModules = Dict.empty
                        }
            in
            case qualifierResult of
                Err qualifierErrors ->
                    formatErrors (QualifierProblem.toString sourceCode) qualifierErrors

                Ok qualifiedAst ->
                    case TypeChecker.run qualifiedAst of
                        Err typeErrors ->
                            formatErrors (TypeCheckerProblem.toString sourceCode) typeErrors

                        Ok typedAst ->
                            Codegen.codegen typedAst
                                |> Result.mapError Debug.toString


formatErrors : (a -> String) -> List a -> Result String b
formatErrors fn problems =
    problems
        |> List.map fn
        |> String.join "\n\n"
        |> Err


subscriptions : Model -> Sub Msg
subscriptions _ =
    compileString CompileString


port compileString : (String -> msg) -> Sub msg


port compileFinished : ( Bool, String ) -> Cmd msg
