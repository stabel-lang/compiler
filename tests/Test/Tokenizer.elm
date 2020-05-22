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
                        type: Int -- Int
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
                        , Metadata "type"
                        , Type "Int"
                        , TypeSeperator
                        , Type "Int"
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
        , test "Data structure without fields" <|
            \_ ->
                let
                    source =
                        """
                        deftype: True
                        """

                    expectedTokens =
                        [ Metadata "deftype"
                        , Type "True"
                        ]
                in
                case tokenize source of
                    Err () ->
                        Expect.fail "Did not expect tokenization to fail"

                    Ok tokens ->
                        Expect.equalLists expectedTokens tokens
        , test "Data structure with fields" <|
            \_ ->
                let
                    source =
                        """
                        deftype: Person
                        : {
                            age: Int
                            jobs: Int
                        }
                        """

                    expectedTokens =
                        [ Metadata "deftype"
                        , Type "Person"
                        , Metadata ""
                        , ListStart
                        , Metadata "age"
                        , Type "Int"
                        , Metadata "jobs"
                        , Type "Int"
                        , ListEnd
                        ]
                in
                case tokenize source of
                    Err () ->
                        Expect.fail "Did not expect tokenization to fail"

                    Ok tokens ->
                        Expect.equalLists expectedTokens tokens
        , test "Quotations" <|
            \_ ->
                let
                    source =
                        """
                        def: apply-to-num
                        type: Int [ Int -- Int ] -- Int
                        : !

                        def: main
                        entry: True
                        : 1 [ 1 + ] apply-to-num
                        """

                    expectedTokens =
                        [ Metadata "def"
                        , Symbol "apply-to-num"
                        , Metadata "type"
                        , Type "Int"
                        , QuoteStart
                        , Type "Int"
                        , TypeSeperator
                        , Type "Int"
                        , QuoteStop
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Symbol "!"

                        -- main
                        , Metadata "def"
                        , Symbol "main"
                        , Metadata "entry"
                        , Type "True"
                        , Metadata ""
                        , Integer 1
                        , QuoteStart
                        , Integer 1
                        , Symbol "+"
                        , QuoteStop
                        , Symbol "apply-to-num"
                        ]
                in
                case tokenize source of
                    Err () ->
                        Expect.fail "Did not expect tokenization to fail"

                    Ok tokens ->
                        Expect.equalLists expectedTokens tokens
        , test "Pattern matching" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: some
                        when: Type( age: 0 ) >True
                        : >False
                        """

                    expectedTokens =
                        [ Metadata "defmulti"
                        , Symbol "some"
                        , Metadata "when"
                        , PatternMatchStart "Type"
                        , Metadata "age"
                        , Integer 0
                        , ParenStop
                        , Symbol ">True"
                        , Metadata ""
                        , Symbol ">False"
                        ]
                in
                case tokenize source of
                    Err () ->
                        Expect.fail "Did not expect tokenization to fail"

                    Ok tokens ->
                        Expect.equalLists expectedTokens tokens
        ]
