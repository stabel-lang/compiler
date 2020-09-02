module Test.Parser.Error exposing (..)

import Expect
import Play.Parser as AST exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (compile)


suite : Test
suite =
    describe "Parser errors"
        [ test "Syntax error" <|
            \_ ->
                let
                    source =
                        """
                            defmulti: origo?
                            when: Pair( 0 0 )
                              >True
                            : >False
                            """
                in
                case compile source of
                    Err () ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Did not expect parsing to succeed"
        ]
