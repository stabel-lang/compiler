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
                                [ QAST.CustomTypeDef "True" [] []
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
                                [ QAST.CustomTypeDef "Person" [] [ ( "age", Type.Int ) ]
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
        , test "Generic custom type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box" [ "a" ] [ ( "element", Type.Generic "a" ) ] ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer 5
                                            , QAST.Word ">Box"
                                            , QAST.Word "element>"
                                            , QAST.Integer 10
                                            , QAST.Builtin Builtin.Plus
                                            , QAST.Integer 15
                                            , QAST.Builtin Builtin.Equal
                                            ]
                                  }
                                , { name = ">Box"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Generic "a" ] [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.ConstructType "Box"
                                            ]
                                  }
                                , { name = ">element"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.CustomGeneric "Box" [ Type.Generic "a" ], Type.Generic "a" ]
                                                [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                  , implementation =
                                        QAST.SoloImpl [ QAST.SetMember "Box" "element" ]
                                  }
                                , { name = "element>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                                [ Type.Generic "a" ]
                                  , implementation =
                                        QAST.SoloImpl [ QAST.GetMember "Box" "element" ]
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
                                []
                                [ Type.Custom "True"
                                , Type.Custom "False"
                                ]
                            , QAST.CustomTypeDef "True" [] []
                            , QAST.CustomTypeDef "False" [] []
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
                                        [ ( QAST.TypeMatch (Type.Custom "False") []
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 0
                                            ]
                                          )
                                        , ( QAST.TypeMatch (Type.Custom "True") []
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
                                        [ ( QAST.TypeMatch (Type.Custom "False") []
                                          , [ QAST.Builtin Builtin.StackDrop
                                            , QAST.Integer 0
                                            ]
                                          )
                                        , ( QAST.TypeMatch (Type.Custom "True") []
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
                                        [ ( QAST.TypeMatch (Type.Custom "False") []
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
            , test "With default branch (no type meta)" <|
                \_ ->
                    let
                        input =
                            template
                                { name = "to-int"
                                , metadata = Metadata.default
                                , implementation =
                                    QAST.MultiImpl
                                        [ ( QAST.TypeMatch (Type.Custom "False") []
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
            , test "When returning union" <|
                \_ ->
                    let
                        input =
                            { types =
                                Dict.fromListBy QAST.typeDefinitionName
                                    [ QAST.UnionTypeDef "Beings"
                                        []
                                        [ Type.Custom "Person"
                                        , Type.Custom "Dog"
                                        ]
                                    , QAST.CustomTypeDef "Person"
                                        []
                                        [ ( "age", Type.Int ) ]
                                    , QAST.CustomTypeDef "Dog"
                                        []
                                        [ ( "man-years", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Person"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Person" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.ConstructType "Person"
                                                ]
                                      }
                                    , { name = ">Dog"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Dog" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.ConstructType "Dog"
                                                ]
                                      }
                                    , { name = "age>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.GetMember "Person" "age"
                                                ]
                                      }
                                    , { name = "man-years>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Dog" ] [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.GetMember "Dog" "man-years"
                                                ]
                                      }
                                    , { name = ">age"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.SetMember "Person" "age"
                                                ]
                                      }
                                    , { name = ">man-years"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Dog", Type.Int ] [ Type.Custom "Dog" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.SetMember "Dog" "man-years"
                                                ]
                                      }
                                    , { name = "add-to-age"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch (Type.Custom "Person") []
                                                  , [ QAST.Word ">age"
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch (Type.Custom "Dog") []
                                                  , [ QAST.Integer 4
                                                    , QAST.Builtin Builtin.Multiply
                                                    , QAST.Word ">man-years"
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = "get-man-age"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch (Type.Custom "Person") []
                                                  , [ QAST.Word "age>" ]
                                                  )
                                                , ( QAST.TypeMatch (Type.Custom "Dog") []
                                                  , [ QAST.Word "man-years>" ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 18
                                                , QAST.Word ">Person"
                                                , QAST.Integer 10
                                                , QAST.Word "add-to-age"
                                                , QAST.Integer 0
                                                , QAST.Word ">Dog"
                                                , QAST.Integer 2
                                                , QAST.Word "add-to-age"
                                                , QAST.Word "get-man-age"
                                                , QAST.Builtin Builtin.StackSwap
                                                , QAST.Word "get-man-age"
                                                , QAST.Builtin Builtin.StackSwap
                                                , QAST.Builtin Builtin.Minus
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
            , test "Function requiring a concrete type should not accept an union with that type" <|
                \_ ->
                    let
                        input =
                            { types =
                                Dict.fromListBy QAST.typeDefinitionName
                                    [ QAST.UnionTypeDef "Bool"
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , QAST.CustomTypeDef "True" [] []
                                    , QAST.CustomTypeDef "False" [] []
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
                                    , { name = "not"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch (Type.Custom "True") []
                                                  , [ QAST.Builtin Builtin.StackDrop
                                                    , QAST.Word ">False"
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch (Type.Custom "False") []
                                                  , [ QAST.Builtin Builtin.StackDrop
                                                    , QAST.Word ">True"
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = "true-to-int"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "True" ] [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin Builtin.StackDrop
                                                , QAST.Integer 1
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Word ">True"
                                                , QAST.Word "not"
                                                , QAST.Word "true-to-int"
                                                ]
                                      }
                                    ]
                            }
                    in
                    case typeCheck input of
                        Ok _ ->
                            Expect.fail "Did not expect type check to pass."

                        Err () ->
                            Expect.pass
            , test "Generic union" <|
                \_ ->
                    let
                        listUnion =
                            [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                            , Type.Custom "EmptyList"
                            ]

                        listUnionInt =
                            [ Type.CustomGeneric "NonEmptyList" [ Type.Int ]
                            , Type.Custom "EmptyList"
                            ]

                        input =
                            { types =
                                Dict.fromListBy QAST.typeDefinitionName
                                    [ QAST.UnionTypeDef "List" [ "a" ] listUnion
                                    , QAST.CustomTypeDef "NonEmptyList"
                                        [ "a" ]
                                        [ ( "element", Type.Generic "a" )
                                        , ( "rest", Type.Union listUnion )
                                        ]
                                    , QAST.CustomTypeDef "EmptyList" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">EmptyList"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "EmptyList" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.ConstructType "EmptyList"
                                                ]
                                      }
                                    , { name = ">NonEmptyList"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Generic "a", Type.Union listUnion ]
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ] ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.ConstructType "NonEmptyList"
                                                ]
                                      }
                                    , { name = "first>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ] ]
                                                    [ Type.Generic "a" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.GetMember "NonEmptyList" "first"
                                                ]
                                      }
                                    , { name = ">first"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ], Type.Generic "a" ]
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ] ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.SetMember "NonEmptyList" "first"
                                                ]
                                      }
                                    , { name = "rest>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ] ]
                                                    [ Type.Union listUnion ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.GetMember "NonEmptyList" "rest"
                                                ]
                                      }
                                    , { name = ">rest"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ], Type.Union listUnion ]
                                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ] ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.SetMember "NonEmptyList" "rest"
                                                ]
                                      }
                                    , { name = "sum"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Union listUnionInt ] [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 0
                                                , QAST.Word "sum-help"
                                                ]
                                      }
                                    , { name = "sum-help"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Union listUnionInt, Type.Int ]
                                                    [ Type.Int ]
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch (Type.CustomGeneric "NonEmptyList" [ Type.Int ]) []
                                                  , [ QAST.Builtin Builtin.StackSwap
                                                    , QAST.Builtin Builtin.StackDuplicate
                                                    , QAST.Word "rest>"
                                                    , QAST.Builtin Builtin.StackRightRotate
                                                    , QAST.Word "first>"
                                                    , QAST.Builtin Builtin.Plus
                                                    , QAST.Word "sum-help"
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch (Type.Custom "EmptyList") []
                                                  , [ QAST.Builtin Builtin.StackSwap
                                                    , QAST.Builtin Builtin.StackDrop
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.Integer 2
                                                , QAST.Integer 3
                                                , QAST.Word ">EmptyList"
                                                , QAST.Word ">NonEmptyList"
                                                , QAST.Word ">NonEmptyList"
                                                , QAST.Word ">NonEmptyList"
                                                , QAST.Word "sum"
                                                , QAST.Integer 6
                                                , QAST.Builtin Builtin.Equal
                                                ]
                                      }
                                    ]
                            }
                    in
                    case typeCheck input of
                        Ok _ ->
                            Expect.pass

                        Err () ->
                            Expect.fail "Expected type check to pass."
            ]
        , describe "Quotations"
            [ test "Simple example" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.WordRef "main__quot2"
                                                , QAST.Word "apply-to-num"
                                                , QAST.WordRef "main__quot1"
                                                , QAST.Word "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.Builtin Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.Builtin Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case typeCheck input of
                        Ok _ ->
                            Expect.pass

                        Err () ->
                            Expect.fail "Did not expect type check to fail."
            , test "With type annotation" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Int
                                                    , Type.Quotation
                                                        { input = [ Type.Int ]
                                                        , output = [ Type.Int ]
                                                        }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.WordRef "main__quot2"
                                                , QAST.Word "apply-to-num"
                                                , QAST.WordRef "main__quot1"
                                                , QAST.Word "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.Builtin Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer 1
                                                , QAST.Builtin Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case typeCheck input of
                        Ok _ ->
                            Expect.pass

                        Err () ->
                            Expect.fail "Did not expect type check to fail."
            ]
        ]
