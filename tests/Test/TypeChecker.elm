module Test.TypeChecker exposing (..)

import Dict
import Expect
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type
import Play.Qualifier as QAST
import Play.TypeChecker exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TypeChecker"
        [ test "Simple program" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "inc"
                                  , { name = "inc"
                                    , metadata = Metadata.default
                                    , implementation =
                                        [ QAST.Integer 1
                                        , QAST.BuiltinPlus
                                        ]
                                    }
                                  )
                                , ( "dec"
                                  , { name = "dec"
                                    , metadata = Metadata.default
                                    , implementation =
                                        [ QAST.Integer 1
                                        , QAST.BuiltinMinus
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                    , implementation =
                                        [ QAST.Integer 1
                                        , QAST.Word "inc"
                                        , QAST.Word "inc"
                                        , QAST.Word "dec"
                                        , QAST.Integer 2
                                        , QAST.BuiltinEqual
                                        ]
                                    }
                                  )
                                ]
                        }

                    expectedResult =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "inc"
                                  , { name = "inc"
                                    , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                    , metadata = Metadata.default
                                    , implementation =
                                        [ IntLiteral 1
                                        , BuiltinPlus
                                        ]
                                    }
                                  )
                                , ( "dec"
                                  , { name = "dec"
                                    , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                    , metadata = Metadata.default
                                    , implementation =
                                        [ IntLiteral 1
                                        , BuiltinMinus
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , type_ = { input = [], output = [ Type.Int ] }
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                    , implementation =
                                        [ IntLiteral 1
                                        , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                        , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                        , Word "dec" { input = [ Type.Int ], output = [ Type.Int ] }
                                        , IntLiteral 2
                                        , BuiltinEqual
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case typeCheck input of
                    Err () ->
                        Expect.fail "Did not expect typecheck to fail."

                    Ok typedAst ->
                        Expect.equal expectedResult typedAst
        , test "Bad type annotation" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "main"
                                  , { name = "main"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] []
                                    , implementation =
                                        [ QAST.Integer 1
                                        , QAST.Integer 2
                                        , QAST.BuiltinEqual
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case typeCheck input of
                    Err () ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Did not expect type check to succeed."
        ]
