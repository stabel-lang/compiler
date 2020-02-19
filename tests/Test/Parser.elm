module Test.Parser exposing (..)

import Expect
import Play.Parser exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parsing source code"
        [ test "Simple math expression" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            [ Def "inc"
                                [ "a" ]
                                [ Symbol "builtin_plus"
                                , Symbol "a"
                                , Integer 1
                                ]
                            ]
                        }
                    )
                    (parse "def inc a = builtin_plus a 1")
        ]
