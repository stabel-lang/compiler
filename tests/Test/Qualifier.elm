module Test.Qualifier exposing (..)

import Expect
import Play.Parser as AST
import Play.Qualifier exposing (..)
import Test exposing (Test, describe, test, todo)


suite : Test
suite =
    describe "Qualifier"
        [ test "Simple program" <|
            \_ ->
                let
                    sourceDefinitions =
                        [ { name = "inc"
                          , metadata = []
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "+"
                                ]
                          }
                        , { name = "dec"
                          , metadata = []
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "-"
                                ]
                          }
                        , { name = "main"
                          , metadata =
                                [ ( "entry", [ AST.Word "true" ] )
                                ]
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "inc"
                                , AST.Word "inc"
                                , AST.Word "dec"
                                , AST.Integer 2
                                , AST.Word "="
                                ]
                          }
                        ]

                    expectedDefinitions =
                        [ { name = "inc"
                          , metadata = []
                          , implementation =
                                [ Integer 1
                                , BuiltinPlus
                                ]
                          }
                        , { name = "dec"
                          , metadata = []
                          , implementation =
                                [ Integer 1
                                , BuiltinMinus
                                ]
                          }
                        , { name = "main"
                          , metadata =
                                [ ( "entry", [ AST.Word "true" ] )
                                ]
                          , implementation =
                                [ Integer 1
                                , Word "inc"
                                , Word "inc"
                                , Word "dec"
                                , Integer 2
                                , BuiltinEqual
                                ]
                          }
                        ]
                in
                case qualify sourceDefinitions of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok definitions ->
                        Expect.equalLists expectedDefinitions definitions
        ]
