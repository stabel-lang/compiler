module Test.Tokenizer exposing (..)

import Expect
import Play.Tokenizer exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tokenizer"
        [ test "Sample program" <|
            \_ ->
                let
                    source =
                        """
                        def: inc
                        : 1 +

                        def: dec
                        : 1 -

                        def: main
                        entry: true
                        : 1 inc inc dec 2 =
                        """

                    expectedTokens =
                        [ -- inc function
                          Metadata "def"
                        , Symbol "inc"
                        , Metadata ""
                        , Integer 1
                        , Symbol "+"

                        -- dec function
                        , Metadata "def"
                        , Symbol "dec"
                        , Metadata ""
                        , Integer 1
                        , Symbol "-"
                        , Metadata "def"

                        -- main function
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Integer 1
                        , Symbol "inc"
                        , Symbol "inc"
                        , Symbol "dec"
                        , Integer 2
                        , Symbol "="
                        ]
                in
                case tokenize source of
                    Err () ->
                        Expect.fail "Did not expect tokenization to fail"

                    Ok tokens ->
                        Expect.equalLists expectedTokens tokens
        ]
