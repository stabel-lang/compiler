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
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer 1
                                            , AST.Word "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer 1
                                            , AST.Word "-"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        AST.SoloImpl
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
                                        SoloImpl
                                            [ Integer 1
                                            , Builtin Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ Integer 1
                                            , Builtin Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
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
                                  , implementation =
                                        AST.SoloImpl
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
                                        SoloImpl
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
        , test "Union types and multifunctions" <|
            \_ ->
                let
                    unqualifiedAst =
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef "Bool"
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , AST.CustomTypeDef "True" [] []
                                , AST.CustomTypeDef "False" [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "False" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "False"
                                            ]
                                  }
                                , { name = "to-int"
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch (Type.Custom "False") [], [ AST.Integer 0 ] )
                                            , ( AST.TypeMatch (Type.Custom "True") [], [ AST.Integer 1 ] )
                                            ]
                                            []
                                  }
                                ]
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" []
                                , CustomTypeDef "False" []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "False" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "False"
                                            ]
                                  }
                                , { name = "to-int"
                                  , metadata = Metadata.default
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch (Type.Custom "False") [], [ Integer 0 ] )
                                            , ( TypeMatch (Type.Custom "True") [], [ Integer 1 ] )
                                            ]
                                            []
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        , describe "Quotations"
            [ test "Basic case" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Int
                                                    , Type.Quotation { input = [ Type.Int ], output = [ Type.Int ] }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Word "!"
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer 1
                                                , AST.Quotation
                                                    [ AST.Integer 1
                                                    , AST.Word "+"
                                                    ]
                                                , AST.Word "apply-to-num"
                                                , AST.Quotation
                                                    [ AST.Integer 1
                                                    , AST.Word "-"
                                                    ]
                                                , AST.Word "apply-to-num"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Int
                                                    , Type.Quotation { input = [ Type.Int ], output = [ Type.Int ] }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , WordRef "main__quot2"
                                                , Word "apply-to-num"
                                                , WordRef "main__quot1"
                                                , Word "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Minus
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
            , test "Do not create new function if quoting exactly one word" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer 1
                                                , AST.Quotation
                                                    [ AST.Word "inc"
                                                    ]
                                                , AST.Word "!"
                                                ]
                                      }
                                    , { name = "inc"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer 1
                                                , AST.Word "+"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , WordRef "inc"
                                                , Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "inc"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Plus
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
        , describe "Pattern matching"
            [ test "Basic example" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef "Bool"
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , AST.CustomTypeDef "True" [] []
                                    , AST.CustomTypeDef "False" [] []
                                    , AST.CustomTypeDef "Box"
                                        []
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">True"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "True" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "True"
                                                ]
                                      }
                                    , { name = ">False"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "False" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "False"
                                                ]
                                      }
                                    , { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Box" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int, Type.Custom "Box" ] [ Type.Custom "Box" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.SetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "<value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Box" ] [ Type.Int ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.GetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch (Type.Custom "Box") [ ( "value", AST.LiteralInt 0 ) ], [ AST.Word ">True" ] )
                                                ]
                                                [ AST.Word ">False" ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types =
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Bool"
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , CustomTypeDef "True" []
                                    , CustomTypeDef "False" []
                                    , CustomTypeDef "Box"
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">True"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "True" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "True"
                                                ]
                                      }
                                    , { name = ">False"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "False" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "False"
                                                ]
                                      }
                                    , { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Box" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int, Type.Custom "Box" ] [ Type.Custom "Box" ]
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "<value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Box" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.Custom "Box") [ ( "value", LiteralInt 0 ) ], [ Word ">True" ] )
                                                ]
                                                [ Word ">False" ]
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
        ]
