module Test.Parser exposing (..)

import Expect
import Play.Parser as AST exposing (..)
import Play.Tokenizer as Token exposing (Token(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser"
        [ test "Sample program" <|
            \_ ->
                let
                    source =
                        [ -- inc function
                          Metadata "def"
                        , Symbol "inc"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "+"

                        -- dec function
                        , Metadata "def"
                        , Symbol "dec"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "-"
                        , Metadata "def"

                        -- main function
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "inc"
                        , Symbol "inc"
                        , Symbol "dec"
                        , Token.Integer 2
                        , Symbol "="
                        ]

                    expectedDefinitions =
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
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok definitions ->
                        Expect.equalLists expectedDefinitions definitions
        ]
