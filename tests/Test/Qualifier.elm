module Test.Qualifier exposing (..)

import Dict
import Expect
import Play.Parser as Parser
import Play.Qualifier exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifying parsed modules"
        [ test "const value" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "num"
                                  , Def "num"
                                        []
                                        [ ConstInt 1 ]
                                  )
                                ]
                        }
                    )
                    (qualify Dict.empty
                        { ast =
                            Dict.fromList
                                [ ( "num"
                                  , Parser.Def "num"
                                        []
                                        [ Parser.Integer 1 ]
                                  )
                                ]
                        }
                    )
        , test "simple function" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Def "inc"
                                        [ "a" ]
                                        [ BuiltinRef IntAdd
                                        , LocalRef "a"
                                        , ConstInt 1
                                        ]
                                  )
                                ]
                        }
                    )
                    (qualify Dict.empty
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Parser.Def "inc"
                                        [ "a" ]
                                        [ Parser.Symbol "builtin_plus"
                                        , Parser.Symbol "a"
                                        , Parser.Integer 1
                                        ]
                                  )
                                ]
                        }
                    )
        , test "function reference" <|
            \_ ->
                Expect.equal
                    (Ok
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Def "inc"
                                        [ "a" ]
                                        [ BuiltinRef IntAdd
                                        , LocalRef "a"
                                        , ConstInt 1
                                        ]
                                  )
                                , ( "callInc"
                                  , Def "callInc"
                                        [ "a" ]
                                        [ ModuleRef "inc"
                                        , LocalRef "a"
                                        ]
                                  )
                                ]
                        }
                    )
                    (qualify Dict.empty
                        { ast =
                            Dict.fromList
                                [ ( "inc"
                                  , Parser.Def "inc"
                                        [ "a" ]
                                        [ Parser.Symbol "builtin_plus"
                                        , Parser.Symbol "a"
                                        , Parser.Integer 1
                                        ]
                                  )
                                , ( "callInc"
                                  , Parser.Def "callInc"
                                        [ "a" ]
                                        [ Parser.Symbol "inc"
                                        , Parser.Symbol "a"
                                        ]
                                  )
                                ]
                        }
                    )
        ]
