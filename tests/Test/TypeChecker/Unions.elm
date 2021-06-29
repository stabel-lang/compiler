module Test.TypeChecker.Unions exposing (suite)

import Dict
import Dict.Extra as Dict
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.Type as Type exposing (Type)
import Stabel.Qualifier as QAST
import Stabel.Qualifier.SourceLocation exposing (emptyRange)
import Stabel.TypeChecker exposing (..)
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as QualifierUtil
import Test.TypeChecker.Util
    exposing
        ( expectAst
        , expectTypeCheck
        , expectTypeCheckFailure
        )


suite : Test
suite =
    describe "TypeChecker -- Unions and multifunctions"
        [ test "Simplest case" <|
            \_ ->
                let
                    input =
                        template
                            { name = "to-int"
                            , metadata = Metadata.default
                            , implementation =
                                QAST.MultiImpl
                                    [ ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 0
                                        ]
                                      )
                                    , ( QAST.TypeMatch emptyRange (Type.Custom "True") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 1
                                        ]
                                      )
                                    ]
                                    []
                            }
                in
                expectTypeCheck input
        , test "With type signature" <|
            \_ ->
                let
                    input =
                        template
                            { name = "to-int"
                            , metadata =
                                Metadata.default
                                    |> Metadata.withType [ boolUnion ] [ Type.Int ]
                            , implementation =
                                QAST.MultiImpl
                                    [ ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 0
                                        ]
                                      )
                                    , ( QAST.TypeMatch emptyRange (Type.Custom "True") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 1
                                        ]
                                      )
                                    ]
                                    []
                            }
                in
                expectTypeCheck input
        , test "With default branch" <|
            \_ ->
                let
                    input =
                        template
                            { name = "to-int"
                            , metadata =
                                Metadata.default
                                    |> Metadata.withType [ boolUnion ] [ Type.Int ]
                            , implementation =
                                QAST.MultiImpl
                                    [ ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 0
                                        ]
                                      )
                                    ]
                                    [ QAST.Builtin emptyRange Builtin.StackDrop
                                    , QAST.Integer emptyRange 1
                                    ]
                            }
                in
                expectTypeCheck input
        , test "With default branch (no type meta)" <|
            \_ ->
                let
                    input =
                        template
                            { name = "to-int"
                            , metadata = Metadata.default
                            , implementation =
                                QAST.MultiImpl
                                    [ ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                      , [ QAST.Builtin emptyRange Builtin.StackDrop
                                        , QAST.Integer emptyRange 0
                                        ]
                                      )
                                    ]
                                    [ QAST.Builtin emptyRange Builtin.StackDrop
                                    , QAST.Integer emptyRange 1
                                    ]
                            }
                in
                expectTypeCheck input
        , test "When returning union" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.UnionTypeDef "Beings"
                                    True
                                    emptyRange
                                    []
                                    [ Type.Custom "Person"
                                    , Type.Custom "Dog"
                                    ]
                                , QAST.CustomTypeDef "Person"
                                    True
                                    emptyRange
                                    []
                                    [ ( "age", Type.Int ) ]
                                , QAST.CustomTypeDef "Dog"
                                    True
                                    emptyRange
                                    []
                                    [ ( "man-years", Type.Int ) ]
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "add-to-age"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.Custom "Person") []
                                              , [ QAST.Function emptyRange ">age"
                                                ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.Custom "Dog") []
                                              , [ QAST.Integer emptyRange 4
                                                , QAST.Builtin emptyRange Builtin.Multiply
                                                , QAST.Function emptyRange ">man-years"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "get-man-age"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.Custom "Person") []
                                              , [ QAST.Function emptyRange "age>" ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.Custom "Dog") []
                                              , [ QAST.Function emptyRange "man-years>" ]
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
                                            [ QAST.Integer emptyRange 18
                                            , QAST.Function emptyRange ">Person"
                                            , QAST.Integer emptyRange 10
                                            , QAST.Function emptyRange "add-to-age"
                                            , QAST.Integer emptyRange 0
                                            , QAST.Function emptyRange ">Dog"
                                            , QAST.Integer emptyRange 2
                                            , QAST.Function emptyRange "add-to-age"
                                            , QAST.Function emptyRange "get-man-age"
                                            , QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Function emptyRange "get-man-age"
                                            , QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Function requiring a concrete type should not accept an union with that type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.UnionTypeDef "Bool"
                                    True
                                    emptyRange
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , QAST.CustomTypeDef "True" True emptyRange [] []
                                , QAST.CustomTypeDef "False" True emptyRange [] []
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "not"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.Custom "True") []
                                              , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                , QAST.Function emptyRange "False"
                                                ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                              , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                , QAST.Function emptyRange "True"
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
                                            [ QAST.Builtin emptyRange Builtin.StackDrop
                                            , QAST.Integer emptyRange 1
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "True"
                                            , QAST.Function emptyRange "not"
                                            , QAST.Function emptyRange "true-to-int"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        , test "Generic union" <|
            \_ ->
                let
                    listUnion =
                        Type.Union
                            [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                            , Type.Custom "EmptyList"
                            ]

                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.UnionTypeDef "List"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                                    , Type.Custom "EmptyList"
                                    ]
                                , QAST.CustomTypeDef "NonEmptyList"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ ( "first", Type.Generic "a" )
                                    , ( "rest", listUnion )
                                    ]
                                , QAST.CustomTypeDef "EmptyList"
                                    True
                                    emptyRange
                                    []
                                    []
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "first-or-default"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ listUnion, Type.Generic "a" ]
                                                [ Type.Generic "a" ]
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]) []
                                              , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                , QAST.Function emptyRange "first>"
                                                ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.Custom "EmptyList") []
                                              , [ QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Builtin emptyRange Builtin.StackDrop
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
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange "EmptyList"
                                            , QAST.Function emptyRange ">NonEmptyList"
                                            , QAST.Integer emptyRange 0
                                            , QAST.Function emptyRange "first-or-default"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Union with generic branch" <|
            \_ ->
                let
                    maybeUnion =
                        Type.Union
                            [ Type.Generic "a"
                            , Type.Custom "Nil"
                            ]

                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.UnionTypeDef "Maybe"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ Type.Generic "a"
                                    , Type.Custom "Nil"
                                    ]
                                , QAST.CustomTypeDef "Nil"
                                    True
                                    emptyRange
                                    []
                                    []
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "with-default"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ maybeUnion, Type.Generic "a" ]
                                                [ Type.Generic "a" ]
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.Generic "a") []
                                              , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.Custom "Nil") []
                                              , [ QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Builtin emptyRange Builtin.StackDrop
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
                                            [ QAST.Function emptyRange "Nil"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange "with-default"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs

                    expectedResult =
                        { types =
                            Dict.fromListBy typeDefName
                                [ UnionTypeDef "Maybe"
                                    emptyRange
                                    [ "a" ]
                                    [ Type.Generic "a"
                                    , Type.Custom "Nil"
                                    ]
                                , CustomTypeDef "Nil" emptyRange [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "Nil"
                                  , type_ = { input = [], output = [ Type.Custom "Nil" ] }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "Nil" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "Nil"
                                            ]
                                  }
                                , { name = "with-default"
                                  , type_ =
                                        { input = [ maybeUnion, Type.Generic "a" ]
                                        , output = [ Type.Generic "a" ]
                                        }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ maybeUnion, Type.Generic "a" ]
                                                [ Type.Generic "a" ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Generic "a") []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "Nil") []
                                              , [ Builtin emptyRange Builtin.StackSwap
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "main"
                                  , type_ = { input = [], output = [ Type.Int ] }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Word emptyRange
                                                "Nil"
                                                { input = []
                                                , output = [ Type.Custom "Nil" ]
                                                }
                                            , IntLiteral emptyRange 1
                                            , Word emptyRange
                                                "with-default"
                                                { input = [ Type.Union [ Type.Int, Type.Custom "Nil" ], Type.Int ]
                                                , output = [ Type.Int ]
                                                }
                                            ]
                                  }
                                ]
                        }
                in
                expectAst input expectedResult
        , test "Generic union fails if not generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.UnionTypeDef "Maybe"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ Type.Generic "b"
                                    , Type.Custom "Nothing"
                                    ]
                                , QAST.CustomTypeDef "Nothing"
                                    True
                                    emptyRange
                                    []
                                    []
                                ]
                        , functions =
                            Dict.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        ]


boolUnion : Type
boolUnion =
    Type.Union
        [ Type.Custom "True"
        , Type.Custom "False"
        ]


template : QAST.FunctionDefinition -> QAST.AST
template multiFn =
    { types =
        Dict.fromListBy QAST.typeDefinitionName
            [ QAST.UnionTypeDef "Bool"
                True
                emptyRange
                []
                [ Type.Custom "True"
                , Type.Custom "False"
                ]
            , QAST.CustomTypeDef "True" True emptyRange [] []
            , QAST.CustomTypeDef "False" True emptyRange [] []
            ]
    , functions =
        Dict.fromListBy .name
            [ multiFn
            , { name = "main"
              , metadata =
                    Metadata.default
                        |> Metadata.asEntryPoint
              , implementation =
                    QAST.SoloImpl
                        [ QAST.Function emptyRange "True"
                        , QAST.Function emptyRange "to-int"
                        , QAST.Function emptyRange "False"
                        , QAST.Function emptyRange "to-int"
                        , QAST.Builtin emptyRange Builtin.Equal
                        ]
              }
            ]
    }
        |> QualifierUtil.addFunctionsForStructs
