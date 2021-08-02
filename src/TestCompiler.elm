port module TestCompiler exposing (main)

import Dict
import Json.Decode as Json
import Platform exposing (Program)
import Result.Extra as Result
import Set
import Stabel.Codegen as Codegen
import Stabel.Parser as Parser
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Problem as ParserProblem
import Stabel.Qualifier as Qualifier
import Stabel.Qualifier.Problem as QualifierProblem
import Stabel.TypeChecker as TypeChecker
import Stabel.TypeChecker.Problem as TypeCheckerProblem
import Stabel.Wasm as Wasm


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
            parserErrors
                |> List.map (Tuple.pair opts.sourceCode)
                |> formatErrors ParserProblem.toString

        Ok ast ->
            let
                qualifierResult =
                    Qualifier.run
                        { packageName = ""
                        , modulePath = ""
                        , ast = ast
                        , externalModules = Dict.empty
                        , inProgressAST = emptyQualifierAst
                        }

                exportedFunctions =
                    Set.singleton opts.entryPoint
            in
            case qualifierResult of
                Err qualifierErrors ->
                    qualifierErrors
                        |> List.map (Tuple.pair opts.sourceCode)
                        |> formatErrors QualifierProblem.toString

                Ok qualifiedAst ->
                    case TypeChecker.run qualifiedAst of
                        Err typeErrors ->
                            typeErrors
                                |> List.map (Tuple.pair opts.sourceCode)
                                |> formatErrors TypeCheckerProblem.toString

                        Ok typedAst ->
                            typedAst
                                |> Codegen.run exportedFunctions
                                |> Ok


emptyQualifierAst : Qualifier.AST
emptyQualifierAst =
    { types = Dict.empty
    , functions = Dict.empty
    , referenceableFunctions = Set.empty
    }


compileProject : CompileProjectOpts -> Result String Wasm.Module
compileProject opts =
    let
        parserResult =
            opts.modules
                |> List.map parseModuleSource
                |> Result.combine

        parseModuleSource mod =
            case Parser.run (mod.package ++ mod.modulePath) mod.source of
                Err errs ->
                    Err ( mod.source, errs )

                Ok ast ->
                    Ok ( mod.package, mod.modulePath, ast )

        sourceDict =
            opts.modules
                |> List.map (\mod -> ( mod.package ++ mod.modulePath, mod.source ))
                |> Dict.fromList
    in
    case parserResult of
        Err ( sourceCode, errs ) ->
            errs
                |> List.map (Tuple.pair sourceCode)
                |> formatErrors ParserProblem.toString

        Ok withAst ->
            let
                initialConfig =
                    { packageName = ""
                    , modulePath = ""
                    , ast =
                        { sourceReference = "test"
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions = Dict.empty
                        }
                    , externalModules = Dict.empty
                    , inProgressAST = emptyQualifierAst
                    }

                qualifierResult =
                    List.foldl qualifyTestTuples ( [], initialConfig ) withAst

                exportedFunctions =
                    Set.singleton opts.entryPoint
            in
            case qualifierResult of
                ( (_ :: _) as qualifierErrors, _ ) ->
                    qualifierErrors
                        |> List.map
                            (\problem ->
                                ( Dict.get
                                    (QualifierProblem.sourceLocationRef problem)
                                    sourceDict
                                    |> Maybe.withDefault ""
                                , problem
                                )
                            )
                        |> formatErrors QualifierProblem.toString

                ( _, qualifiedAst ) ->
                    case TypeChecker.run qualifiedAst.inProgressAST of
                        Err typeErrors ->
                            typeErrors
                                |> List.map
                                    (\problem ->
                                        ( Dict.get
                                            (TypeCheckerProblem.sourceLocationRef problem)
                                            sourceDict
                                            |> Maybe.withDefault ""
                                        , problem
                                        )
                                    )
                                |> formatErrors TypeCheckerProblem.toString

                        Ok typedAst ->
                            typedAst
                                |> Codegen.run exportedFunctions
                                |> Ok


qualifyTestTuples :
    ( String, String, Parser.AST )
    -> ( List QualifierProblem.Problem, Qualifier.RunConfig )
    -> ( List QualifierProblem.Problem, Qualifier.RunConfig )
qualifyTestTuples ( packageName, modulePath, parserAst ) ( problems, config ) =
    let
        updatedConfig =
            { config
                | packageName = packageName
                , modulePath = modulePath
                , ast = parserAst
            }
    in
    case Qualifier.run updatedConfig of
        Err errs ->
            ( errs ++ problems, updatedConfig )

        Ok qualifiedAST ->
            let
                inProgressAST =
                    updatedConfig.inProgressAST

                updatedInProgressAst =
                    { inProgressAST
                        | types = Dict.union qualifiedAST.types inProgressAST.types
                        , functions = Dict.union qualifiedAST.functions inProgressAST.functions
                        , referenceableFunctions = Set.union qualifiedAST.referenceableFunctions inProgressAST.referenceableFunctions
                    }

                configWithQualifiedAst =
                    { updatedConfig
                        | inProgressAST = updatedInProgressAst
                        , externalModules = Dict.insert ("/" ++ modulePath) packageName updatedConfig.externalModules
                    }
            in
            ( problems, configWithQualifiedAst )


formatErrors : (String -> a -> String) -> List ( String, a ) -> Result String b
formatErrors fn problems =
    problems
        |> List.map (\( source, err ) -> fn source err)
        |> String.join "\n\n"
        |> Err


compileSucceded : Wasm.Module -> Cmd msg
compileSucceded wasm =
    compileFinished ( True, Wasm.toString wasm )


compileFailed : String -> Cmd msg
compileFailed err =
    compileFinished ( False, "Compilation failed:\n\n" ++ err )


port compileFinished : ( Bool, String ) -> Cmd msg
