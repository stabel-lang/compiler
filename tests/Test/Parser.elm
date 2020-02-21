module Test.Parser exposing (..)

import Dict
import Expect
import Play.Parser exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parsing source code"
        [ test "const value" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "num"
                                  , Def "num"
                                        []
                                        [ Integer 10 ]
                                  )
                                ]
                        }
                    )
                    (parse "def num = 10")
        , test "Simple inc function" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Def "inc"
                                        [ "a" ]
                                        [ Symbol "builtin_plus"
                                        , Symbol "a"
                                        , Integer 1
                                        ]
                                  )
                                ]
                        }
                    )
                    (parse "def inc a = builtin_plus a 1")
        , test "Multiple defs" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Def "inc"
                                        [ "a" ]
                                        [ Symbol "builtin_plus"
                                        , Symbol "a"
                                        , Integer 1
                                        ]
                                  )
                                , ( "dec"
                                  , Def "dec"
                                        [ "a" ]
                                        [ Symbol "builtin_sub"
                                        , Symbol "a"
                                        , Integer 1
                                        ]
                                  )
                                ]
                        }
                    )
                    (parse "def inc a = builtin_plus a 1 def dec a = builtin_sub a 1")
        ]
