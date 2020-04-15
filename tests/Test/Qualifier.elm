module Test.Qualifier exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Builtin as Builtin
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type
import Play.Parser as AST
import Play.Qualifier exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier"
        [ test "Simple program" <|
            \_ ->
                let
                    unqualifiedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "+"
                                        ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "-"
                                        ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , whens = []
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
                        }

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ Integer 1
                                        , Builtin Builtin.Plus
                                        ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        [ Integer 1
                                        , Builtin Builtin.Minus
                                        ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        [ Integer 1
                                        , Word "inc"
                                        , Word "inc"
                                        , Word "dec"
                                        , Integer 2
                                        , Builtin Builtin.Equal
                                        ]
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        , test "Generic function types" <|
            \_ ->
                let
                    unqualifiedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "a", Type.Generic "b", Type.Generic "a" ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Word "swap"
                                        , AST.Word "dup"
                                        , AST.Word "rotate"
                                        ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a_over", Type.Generic "b_over" ]
                                                [ Type.Generic "a_over"
                                                , Type.Generic "b_over"
                                                , Type.Generic "a_over"
                                                ]
                                  , implementation =
                                        [ Builtin Builtin.StackSwap
                                        , Builtin Builtin.StackDuplicate
                                        , Builtin Builtin.StackRightRotate
                                        ]
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        ]
