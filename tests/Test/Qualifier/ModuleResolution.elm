module Test.Qualifier.ModuleResolution exposing (suite)

import Dict
import Expect exposing (Expectation)
import Set
import Stabel.Parser as Parser
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Problem as Parser
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem as Problem exposing (Problem)
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as Util


suite : Test
suite =
    describe "Qualifier -- Module resolution"
        [ test "Referencing a function from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/test"
                          , "internal/mod"
                          , """
                            defmodule:
                            exposing: dummy
                            :

                            def: value
                            : 1

                            def: dummy
                            : 1
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defmodule:
                            import: internal/mod
                            :

                            def: main
                            : value
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.FunctionNotExposed _ "/stabel/test/internal/mod/value" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a function from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/external"
                          , "mod"
                          , """
                            defmodule:
                            exposing: dummy
                            :

                            def: add
                            : 1

                            def: dummy
                            : 1
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defmodule:
                            import: /mod
                            :

                            def: main
                            : 1 2 add
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.FunctionNotExposed _ "/stabel/external/mod/add" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type signature from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/external"
                          , "mod"
                          , """
                            defmodule:
                            exposing: dummy
                            :

                            defstruct: Tipe

                            def: dummy
                            : 1
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            def: main
                            type: /mod/Tipe --
                            : drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type definition from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/external"
                          , "mod"
                          , """
                            defmodule:
                            exposing: Tipe
                            :

                            defunion: TipeUnion
                            : a
                            : Tipe

                            defstruct: Tipe
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defstruct: BoxedTipe
                            : value /mod/TipeUnion
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/TipeUnion" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type match from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/external"
                          , "mod"
                          , """
                            defmodule:
                            exposing: Dummy
                            :

                            defstruct: Tipe
                            defstruct: Dummy
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defmulti: call
                            : /mod/Tipe
                              drop
                            else: drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type signature from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/test"
                          , "mod"
                          , """
                            defmodule:
                            exposing: Dummy
                            :

                            defstruct: Tipe
                            defstruct: Dummy
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            def: call
                            type: mod/Tipe --
                            : drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type definition from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/test"
                          , "mod"
                          , """
                            defmodule:
                            exposing: Tipe
                            :

                            defunion: TipeUnion
                            : Tipe
                            : a

                            defstruct: Tipe
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defstruct: BoxedTipe
                            : value mod/TipeUnion
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/TipeUnion" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type match from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "stabel/test"
                          , "mod"
                          , """
                            defmodule:
                            exposing: TipeUnion
                            :

                            defunion: TipeUnion
                            : Tipe
                            : a

                            defstruct: Tipe
                            """
                          )
                        , ( "stabel/test"
                          , "core"
                          , """
                            defmulti: call
                            : mod/Tipe
                              drop
                            else: drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        ]


checkForError : (Problem -> Bool) -> List ( String, String, String ) -> Expectation
checkForError pred sources =
    let
        parserResult =
            sources
                |> List.map (\( a, name, source ) -> ( a, name, Parser.run name source ))
                |> collectErrors
    in
    case parserResult of
        Err errs ->
            Expect.fail <| "Parse error: " ++ Debug.toString errs

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
                    , inProgressAST = Util.emptyAst
                    }

                qualifyResult =
                    withAst
                        |> List.foldl qualifyTestTuples ( [], initialConfig )
                        |> Tuple.first
            in
            case qualifyResult of
                [] ->
                    Expect.fail "Expected error."

                errs ->
                    if List.any pred errs then
                        Expect.pass

                    else
                        Expect.fail <| "Failed for unknown qualification error: " ++ Debug.toString errs


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird fn ( a, b, c ) =
    ( a, b, fn c )


collectErrors :
    List ( a, b, Result e o )
    -> Result (List e) (List ( a, b, o ))
collectErrors tuples =
    case List.foldr collectErrorsHelper ( [], [] ) tuples of
        ( [], oks ) ->
            Ok oks

        ( errs, _ ) ->
            Err errs


collectErrorsHelper :
    ( a, b, Result e o )
    -> ( List e, List ( a, b, o ) )
    -> ( List e, List ( a, b, o ) )
collectErrorsHelper ( a, b, result ) ( errors, oks ) =
    case result of
        Err error ->
            ( error :: errors, oks )

        Ok ast ->
            ( errors, ( a, b, ast ) :: oks )


qualifyTestTuples :
    ( String, String, Parser.AST )
    -> ( List Problem, RunConfig )
    -> ( List Problem, RunConfig )
qualifyTestTuples ( packageName, modulePath, parserAst ) ( problems, config ) =
    let
        updatedConfig =
            { config
                | packageName = packageName
                , modulePath = modulePath
                , ast = parserAst
            }
    in
    case run updatedConfig of
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
