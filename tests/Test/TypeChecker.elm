module Test.TypeChecker exposing (..)

import Expect
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type
import Play.Qualifier as QAST
import Play.TypeChecker exposing (..)
import Test exposing (Test, describe, test)


defaultMeta : Metadata
defaultMeta =
    Metadata.default


suite : Test
suite =
    describe "TypeChecker"
        [ test "Simple program" <|
            \_ ->
                let
                    entryMeta =
                        { defaultMeta | isEntryPoint = True }

                    input =
                        [ { name = "inc"
                          , metadata = defaultMeta
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , metadata = defaultMeta
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , metadata = entryMeta
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.Word "inc"
                                , QAST.Word "inc"
                                , QAST.Word "dec"
                                , QAST.Integer 2
                                , QAST.BuiltinEqual
                                ]
                          }
                        ]

                    expectedResult =
                        [ { name = "inc"
                          , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                          , metadata = defaultMeta
                          , implementation =
                                [ IntLiteral 1
                                , BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                          , metadata = defaultMeta
                          , implementation =
                                [ IntLiteral 1
                                , BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , type_ = { input = [], output = [ Type.Int ] }
                          , metadata = entryMeta
                          , implementation =
                                [ IntLiteral 1
                                , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                , Word "dec" { input = [ Type.Int ], output = [ Type.Int ] }
                                , IntLiteral 2
                                , BuiltinEqual
                                ]
                          }
                        ]
                            |> List.sortBy .name
                in
                case typeCheck input of
                    Err () ->
                        Expect.fail "Did not expect typecheck to fail."

                    Ok typedAst ->
                        Expect.equalLists expectedResult typedAst
        , test "Bad type annotation" <|
            \_ ->
                let
                    input =
                        [ { name = "main"
                          , metadata = { defaultMeta | type_ = Just { input = [ Type.Int ], output = [] } }
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.Integer 2
                                , QAST.BuiltinEqual
                                ]
                          }
                        ]
                in
                case typeCheck input of
                    Err () ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Did not expect type check to succeed."
        ]
