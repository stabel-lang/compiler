module Test.Parser exposing (..)

import Expect
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type
import Play.Parser as AST exposing (..)
import Play.Tokenizer as Token exposing (Token(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser"
        [ test "Sample program" <|
            \_ ->
                let
                    defaultMeta =
                        Metadata.default

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
                        , Metadata "type"
                        , Type "Int"
                        , TypeSeperator
                        , Type "Int"
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
                          , metadata = Metadata.default
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "+"
                                ]
                          }
                        , { name = "dec"
                          , metadata = { defaultMeta | type_ = Just { input = [ Type.Int ], output = [ Type.Int ] } }
                          , implementation =
                                [ AST.Integer 1
                                , AST.Word "-"
                                ]
                          }
                        , { name = "main"
                          , metadata = { defaultMeta | isEntryPoint = True }
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
