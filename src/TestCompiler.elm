port module TestCompiler exposing (main)

import Dict
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


main : Program Input Model msg
main =
    Platform.worker
        { init = \input -> ( (), init input )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        }


init : Input -> Cmd msg
init input =
    case input of
        CompileString opts ->
            case compileString opts of
                Ok wasm ->
                    compileSucceded wasm

                Err errmsg ->
                    compileFailed errmsg

        CompileProject opts ->
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
