port module TestCompiler exposing (main)

import Dict
import Json.Decode as Json
import Platform exposing (Program)
import Set
import Stabel.Codegen as Codegen
import Stabel.Parser as Parser
import Stabel.Parser.Problem as ParserProblem
import Stabel.Qualifier as Qualifier
import Stabel.Qualifier.Problem as QualifierProblem
import Stabel.TypeChecker as TypeChecker
import Stabel.TypeChecker.Problem as TypeCheckerProblem
import Wasm


type alias Model =
    ()


type Input
    = CompileString CompileStringOpts
    | CompileProject CompileProjectOpts


type alias CompileStringOpts =
    { entryPoint : String
    , sourceCode : String
    }


type alias CompileProjectOpts =
    { entryPoint : String
    , modules : List ModuleSource
    }


type alias ModuleSource =
    { package : String
    , modulePath : String
    , source : String
    }



-- Json Decoding --


decodeInput : Json.Value -> Result Json.Error Input
decodeInput json =
    Json.decodeValue inputDecoder json


inputDecoder : Json.Decoder Input
inputDecoder =
    let
        specializedDecoder type_ =
            case String.toUpper type_ of
                "COMPILESTRING" ->
                    compileStringOptsDecoder

                "COMPILEPROJECT" ->
                    compileProjectOptsDecoder

                _ ->
                    Json.fail <| "Unknown compilation mode: " ++ type_
    in
    Json.field "__type" Json.string
        |> Json.andThen specializedDecoder


compileStringOptsDecoder : Json.Decoder Input
compileStringOptsDecoder =
    Json.map CompileString <|
        Json.map2 CompileStringOpts
            (Json.field "entryPoint" Json.string)
            (Json.field "sourceCode" Json.string)


compileProjectOptsDecoder : Json.Decoder Input
compileProjectOptsDecoder =
    Json.map CompileProject <|
        Json.map2 CompileProjectOpts
            (Json.field "entryPoint" Json.string)
            (Json.field "modules" (Json.list moduleSourceDecoder))


moduleSourceDecoder : Json.Decoder ModuleSource
moduleSourceDecoder =
    Json.map3 ModuleSource
        (Json.field "package" Json.string)
        (Json.field "module" Json.string)
        (Json.field "source" Json.string)



-- Main Logic --


main : Program Json.Value Model msg
main =
    Platform.worker
        { init = \input -> ( (), init input )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        }


init : Json.Value -> Cmd msg
init input =
    case decodeInput input of
        Err err ->
            compileFailed <| "Something is wrong with the input: " ++ Json.errorToString err

        Ok (CompileString opts) ->
            case compileString opts of
                Ok wasm ->
                    compileSucceded wasm

                Err errmsg ->
                    compileFailed errmsg

        Ok (CompileProject opts) ->
            case compileProject opts of
                Ok wasm ->
                    compileSucceded wasm

                Err errmsg ->
                    compileFailed errmsg


compileString : CompileStringOpts -> Result String Wasm.Module
compileString opts =
    case Parser.run "test" opts.sourceCode of
        Err parserErrors ->
            formatErrors (ParserProblem.toString opts.sourceCode) parserErrors

        Ok ast ->
            let
                qualifierResult =
                    Qualifier.run
                        { packageName = ""
                        , modulePath = ""
                        , ast = ast
                        , externalModules = Dict.empty
                        , inProgressAST =
                            { types = Dict.empty
                            , functions = Dict.empty
                            , referenceableFunctions = Set.empty
                            }
                        }

                exportedFunctions =
                    Set.singleton opts.entryPoint
            in
            case qualifierResult of
                Err qualifierErrors ->
                    formatErrors (QualifierProblem.toString opts.sourceCode) qualifierErrors

                Ok qualifiedAst ->
                    case TypeChecker.run qualifiedAst of
                        Err typeErrors ->
                            formatErrors (TypeCheckerProblem.toString opts.sourceCode) typeErrors

                        Ok typedAst ->
                            typedAst
                                |> Codegen.run exportedFunctions
                                |> Ok


compileProject : CompileProjectOpts -> Result String Wasm.Module
compileProject opts =
    Debug.todo "TODO"


formatErrors : (a -> String) -> List a -> Result String b
formatErrors fn problems =
    problems
        |> List.map fn
        |> String.join "\n\n"
        |> Err


compileSucceded : Wasm.Module -> Cmd msg
compileSucceded wasm =
    compileFinished ( True, Wasm.toString wasm )


compileFailed : String -> Cmd msg
compileFailed err =
    compileFinished ( False, "Compilation failed:\n\n" ++ err )


port compileFinished : ( Bool, String ) -> Cmd msg
