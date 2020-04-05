module Test.TypeChecker exposing (..)

import Dict
import Dict.Extra as Dict
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
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ QAST.Integer 1
                                        , QAST.BuiltinPlus
                                        ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ QAST.Integer 1
                                        , QAST.BuiltinMinus
                                        ]
                                  }
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
                                ]
                        }

                    expectedResult =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ IntLiteral 1
                                        , BuiltinPlus
                                        ]
                                  }
                                , { name = "dec"
                                  , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ IntLiteral 1
                                        , BuiltinMinus
                                        ]
                                  }
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
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] []
                                  , implementation =
                                        [ QAST.Integer 1
                                        , QAST.Integer 2
                                        , QAST.BuiltinEqual
                                        ]
                                  }
                                ]
                        }
                in
                case typeCheck input of
                    Err () ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Did not expect type check to succeed."
        , test "Custom data structure without fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromList
                                [ ( "True"
                                  , { name = "True"
                                    , members = []
                                    }
                                  )
                                ]
                        , words =
                            Dict.fromList
                                [ ( ">True"
                                  , { name = ">True"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                    , implementation = [ QAST.ConstructType "True" ]
                                    }
                                  )
                                , ( "as-int"
                                  , { name = "as-int"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                    , implementation =
                                        [ QAST.Integer 1
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                    , implementation =
                                        [ QAST.Word ">True"
                                        , QAST.Word "as-int"
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case typeCheck source of
                    Err () ->
                        Expect.fail "Did not expect type check to fail"

                    Ok _ ->
                        Expect.pass
        , test "Custom data structure with fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromList
                                [ ( "Person"
                                  , { name = "Person"
                                    , members = [ ( "age", Type.Int ) ]
                                    }
                                  )
                                ]
                        , words =
                            Dict.fromList
                                [ ( ">Person"
                                  , { name = ">Person"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Custom "Person" ]
                                    , implementation = [ QAST.ConstructType "Person" ]
                                    }
                                  )
                                , ( ">age"
                                  , { name = ">age"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                    , implementation = [ QAST.SetMember "Person" "age" ]
                                    }
                                  )
                                , ( "age>"
                                  , { name = "age>"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                    , implementation = [ QAST.GetMember "Person" "age" ]
                                    }
                                  )
                                , ( "inc-age"
                                  , { name = "inc-age"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Custom "Person" ]
                                    , implementation =
                                        [ QAST.Word "age>"
                                        , QAST.Integer 1
                                        , QAST.BuiltinPlus
                                        , QAST.Word ">Person"
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
                                        , QAST.Word ">Person"
                                        , QAST.Word "inc-age"
                                        , QAST.Word "age>"
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case typeCheck source of
                    Err () ->
                        Expect.fail "Did not expect type check to fail"

                    Ok _ ->
                        Expect.pass
        ]
