module Test.TypeChecker exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Builtin as Builtin
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
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Builtin Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Builtin Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Word "inc"
                                            , QAST.Word "inc"
                                            , QAST.Word "dec"
                                            , QAST.Integer 2
                                            , QAST.Builtin Builtin.Equal
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
                                        SoloImpl
                                            [ IntLiteral 1
                                            , Builtin Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ IntLiteral 1
                                            , Builtin Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , type_ = { input = [], output = [ Type.Int ] }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ IntLiteral 1
                                            , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , Word "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , Word "dec" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , IntLiteral 2
                                            , Builtin Builtin.Equal
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
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Integer 2
                                            , QAST.Builtin Builtin.Equal
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
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "True" []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.ConstructType "True" ]
                                  }
                                , { name = "as-int"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Word ">True"
                                            , QAST.Word "as-int"
                                            ]
                                  }
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
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Person" [ ( "age", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">Person"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Custom "Person" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.ConstructType "Person" ]
                                  }
                                , { name = ">age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.SetMember "Person" "age" ]
                                  }
                                , { name = "age>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.GetMember "Person" "age" ]
                                  }
                                , { name = "inc-age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Custom "Person" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Word "age>"
                                            , QAST.Integer 1
                                            , QAST.Builtin Builtin.Plus
                                            , QAST.Word ">Person"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Word ">Person"
                                            , QAST.Word "inc-age"
                                            , QAST.Word "age>"
                                            ]
                                  }
                                ]
                        }
                in
                case typeCheck source of
                    Err () ->
                        Expect.fail "Did not expect type check to fail"

                    Ok _ ->
                        Expect.pass
        , test "Generic types" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 1
                                            , QAST.Integer 2
                                            , QAST.Word "over"
                                            , QAST.Builtin Builtin.Plus
                                            , QAST.Builtin Builtin.Minus
                                            , QAST.Integer 2
                                            , QAST.Builtin Builtin.Equal
                                            ]
                                  }
                                , { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                -- Most would start at a and increment, but we need to check that
                                                -- the typechecker cares about the relationship between these generic
                                                -- variables, not the names themselves
                                                [ Type.Generic "b_over", Type.Generic "c_over" ]
                                                [ Type.Generic "b_over", Type.Generic "c_over", Type.Generic "b_over" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin Builtin.StackSwap
                                            , QAST.Builtin Builtin.StackDuplicate
                                            , QAST.Builtin Builtin.StackRightRotate
                                            ]
                                  }
                                ]
                        }
                in
                case typeCheck input of
                    Err () ->
                        Expect.fail "Did not expect type check to fail."

                    Ok _ ->
                        Expect.pass
        , test "Generic types with type annotation" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 5
                                            , QAST.Word "square"
                                            ]
                                  }
                                , { name = "square"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin Builtin.StackDuplicate
                                            , QAST.Builtin Builtin.Multiply
                                            ]
                                  }
                                ]
                        }
                in
                case typeCheck input of
                    Err () ->
                        Expect.fail "Did not expect type check to fail."

                    Ok _ ->
                        Expect.pass
        , describe "Unions and multifunctions" <|
            let
                template multiFn =
                    { types =
                        Dict.fromListBy QAST.typeDefinitionName
                            [ QAST.UnionTypeDef "Bool"
                                [ Type.Custom "True"
                                , Type.Custom "False"
                                ]
                            , QAST.CustomTypeDef "True" []
                            , QAST.CustomTypeDef "False" []
                            ]
                    , words =
                        Dict.fromListBy .name
                            [ { name = ">True"
                              , metadata =
                                    Metadata.default
                                        |> Metadata.withType [] [ Type.Custom "True" ]
                              , implementation =
                                    QAST.SoloImpl
                                        [ QAST.ConstructType "True"
                                        ]
                              }
                            , { name = ">False"
                              , metadata =
                                    Metadata.default
                                        |> Metadata.withType [] [ Type.Custom "False" ]
                              , implementation =
                                    QAST.SoloImpl
                                        [ QAST.ConstructType "False"
                                        ]
                              }
                            , multiFn
                            , { name = "main"
                              , metadata =
                                    Metadata.default
                                        |> Metadata.asEntryPoint
                              , implementation =
                                    QAST.SoloImpl
                                        [ QAST.Word ">True"
                                        , QAST.Word "to-int"
                                        , QAST.Word ">False"
                                        , QAST.Word "to-int"
                                        , QAST.Builtin Builtin.Equal
                                        ]
                              }
                            ]
                    }
            in
            [ test "Simplest case" <|
                \_ ->
                    let
                        input =
                            template
                                { name = "to-int"
                                , metadata = Metadata.default
                                , implementation =
                                    QAST.MultiImpl
                                        [ ( Type.Custom "False"
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 0
                                            ]
                                          )
                                        , ( Type.Custom "True"
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 1
                                            ]
                                          )
                                        ]
                                        []
                                }
                    in
                    case typeCheck input of
                        Err () ->
                            Expect.fail "Did not expect type check to fail."

                        Ok _ ->
                            Expect.pass
            , test "With type signature" <|
                \_ ->
                    let
                        input =
                            template
                                { name = "to-int"
                                , metadata =
                                    Metadata.default
                                        |> Metadata.withType [ Type.Custom "Bool" ] [ Type.Int ]
                                , implementation =
                                    QAST.MultiImpl
                                        [ ( Type.Custom "False"
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 0
                                            ]
                                          )
                                        , ( Type.Custom "True"
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 1
                                            ]
                                          )
                                        ]
                                        []
                                }
                    in
                    case typeCheck input of
                        Err () ->
                            Expect.fail "Did not expect type check to fail."

                        Ok _ ->
                            Expect.pass
            , test "With default branch" <|
                \_ ->
                    let
                        input =
                            template
                                { name = "to-int"
                                , metadata =
                                    Metadata.default
                                        |> Metadata.withType [ Type.Custom "Bool" ] [ Type.Int ]
                                , implementation =
                                    QAST.MultiImpl
                                        [ ( Type.Custom "False"
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 0
                                            ]
                                          )
                                        ]
                                        [ QAST.Builtin Builtin.StackDrop
                                        , QAST.Integer 1
                                        ]
                                }
                    in
                    case typeCheck input of
                        Err () ->
                            Expect.fail "Did not expect type check to fail."

                        Ok _ ->
                            Expect.pass
            ]
        ]
