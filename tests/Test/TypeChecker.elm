module Test.TypeChecker exposing (..)

import Expect
import Play.Parser as PAST
import Play.Qualifier as QAST
import Play.TypeChecker exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TypeChecker"
        [ test "Simple program" <|
            \_ ->
                let
                    input =
                        [ { name = "inc"
                          , metadata = []
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , metadata = []
                          , implementation =
                                [ QAST.Integer 1
                                , QAST.BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , metadata =
                                [ ( "entry", [ PAST.Word "true" ] )
                                ]
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
                          , type_ = { input = [ IntType ], output = [ IntType ] }
                          , metadata = []
                          , implementation =
                                [ IntLiteral 1
                                , BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , type_ = { input = [ IntType ], output = [ IntType ] }
                          , metadata = []
                          , implementation =
                                [ IntLiteral 1
                                , BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , type_ = { input = [], output = [ IntType ] }
                          , metadata =
                                [ ( "entry", [ PAST.Word "true" ] )
                                ]
                          , implementation =
                                [ IntLiteral 1
                                , Word "inc" { input = [ IntType ], output = [ IntType ] }
                                , Word "inc" { input = [ IntType ], output = [ IntType ] }
                                , Word "dec" { input = [ IntType ], output = [ IntType ] }
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
        ]
