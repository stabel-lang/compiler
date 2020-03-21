module Test.Codegen exposing (..)

import Expect
import Play.Codegen exposing (..)
import Play.Parser as UnqualifiedAST
import Play.Qualifier as AST
import Test exposing (Test, describe, test)
import Wasm


suite : Test
suite =
    describe "Codegen"
        [ test "Simple program" <|
            \_ ->
                let
                    ast =
                        [ { name = "inc"
                          , metadata = []
                          , implementation =
                                [ AST.Integer 1
                                , AST.BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , metadata = []
                          , implementation =
                                [ AST.Integer 1
                                , AST.BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , metadata =
                                [ ( "entry", [ UnqualifiedAST.Word "true" ] )
                                ]
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "inc"
                                , AST.Word "inc"
                                , AST.Word "dec"
                                , AST.Integer 2
                                , AST.BuiltinEqual
                                ]
                          }
                        ]
                in
                case codegen ast of
                    Err () ->
                        Expect.fail "Did not expect codegen to fail"

                    Ok wasmModule ->
                        Expect.fail (Wasm.toString wasmModule)
        ]
