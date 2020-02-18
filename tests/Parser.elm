module Parser exposing (..)

import Expect
import Play.Parser exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parsing source code"
        [ test "Simple math expression" <|
            \_ ->
                Expect.equal
                    { ast =
                        [ Def "add"
                            [ "a", "b" ]
                            [ Symbol "a"
                            , Symbol "+"
                            , Symbol "b"
                            ]
                        ]
                    }
                    (parse "def add a b = a + b")
        ]
