module Test.TypeChecker exposing (suite)

import Dict
import Dict.Extra as Dict
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Qualifier as QAST
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
    describe "TypeChecker"
        [ test "Simple program" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange "inc"
                                            , QAST.Function emptyRange "inc"
                                            , QAST.Function emptyRange "dec"
                                            , QAST.Integer emptyRange 2
                                            , QAST.Builtin emptyRange Builtin.Equal
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
                                            [ IntLiteral emptyRange 1
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , type_ = { input = [ Type.Int ], output = [ Type.Int ] }
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ IntLiteral emptyRange 1
                                            , Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , type_ = { input = [], output = [ Type.Int ] }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ IntLiteral emptyRange 1
                                            , Word emptyRange "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , Word emptyRange "inc" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , Word emptyRange "dec" { input = [ Type.Int ], output = [ Type.Int ] }
                                            , IntLiteral emptyRange 2
                                            , Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                in
                expectAst input expectedResult
        , test "Bad type annotation" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] []
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                in
                expectTypeCheckFailure input
        , test "Custom data structure without fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "True" True emptyRange [] []
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "as-int"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "True"
                                            , QAST.Function emptyRange "as-int"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck source
        , test "Custom data structure with fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Person" True emptyRange [] [ ( "age", Type.Int ) ]
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc-age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Custom "Person" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "age>"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            , QAST.Function emptyRange ">Person"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange ">Person"
                                            , QAST.Function emptyRange "inc-age"
                                            , QAST.Function emptyRange "age>"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck source
        , test "Generic types" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Function emptyRange "over"
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            , QAST.Builtin emptyRange Builtin.Minus
                                            , QAST.Integer emptyRange 2
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                , { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                -- Most would start at a and increment, but we need to check that
                                                -- the typechecker cares about the relationship between these generic
                                                -- variables, not the names themselves
                                                [ Type.Generic "b", Type.Generic "c" ]
                                                [ Type.Generic "b", Type.Generic "c", Type.Generic "b" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Builtin emptyRange Builtin.StackRightRotate
                                            ]
                                  }
                                ]
                        }
                in
                expectTypeCheck input
        , test "Generic types with type annotation" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 5
                                            , QAST.Function emptyRange "square"
                                            ]
                                  }
                                , { name = "square"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Builtin emptyRange Builtin.Multiply
                                            ]
                                  }
                                ]
                        }
                in
                expectTypeCheck input
        , test "Generic custom type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ ( "element", Type.Generic "a" ) ]
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 5
                                            , QAST.Function emptyRange ">Box"
                                            , QAST.Function emptyRange "element>"
                                            , QAST.Integer emptyRange 10
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            , QAST.Integer emptyRange 15
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Generic types with rotations and quotations" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Coordinate"
                                    True
                                    emptyRange
                                    []
                                    [ ( "x", Type.Int )
                                    , ( "y", Type.Int )
                                    ]
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Function emptyRange ">Coordinate"
                                            , QAST.FunctionRef emptyRange "main__quot1"
                                            , QAST.Function emptyRange "update-x"
                                            , QAST.Function emptyRange "x>"
                                            ]
                                  }
                                , { name = "main__quot1"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "update-x"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Custom "Coordinate"
                                                , Type.FunctionSignature { input = [ Type.Int ], output = [ Type.Int ] }
                                                ]
                                                [ Type.Custom "Coordinate" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Function emptyRange "x>"
                                            , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                            , QAST.Builtin emptyRange Builtin.Apply
                                            , QAST.Function emptyRange ">x"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Generic types with generic quotations" <|
            \_ ->
                let
                    listUnion =
                        [ Type.Custom "Empty"
                        , Type.CustomGeneric "NonEmpty" [ Type.Generic "a" ]
                        ]

                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Pair"
                                    True
                                    emptyRange
                                    [ "a", "b" ]
                                    [ ( "first", Type.Generic "a" )
                                    , ( "second", Type.Generic "b" )
                                    ]
                                , QAST.UnionTypeDef "List"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    listUnion
                                , QAST.CustomTypeDef "NonEmpty"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ ( "head", Type.Generic "a" )
                                    , ( "rest", Type.Union (Just "List") listUnion )
                                    ]
                                , QAST.CustomTypeDef "Empty" True emptyRange [] []
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Integer emptyRange 3
                                            , QAST.Function emptyRange "Empty"
                                            , QAST.Function emptyRange ">NonEmpty"
                                            , QAST.Function emptyRange ">NonEmpty"
                                            , QAST.Function emptyRange ">NonEmpty"
                                            , QAST.Integer emptyRange 0
                                            , QAST.FunctionRef emptyRange "main__quot1"
                                            , QAST.Function emptyRange "fold"
                                            ]
                                  }
                                , { name = "main__quot1"
                                  , metadata = Metadata.default
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "fold"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Union (Just "List") listUnion
                                                , Type.Generic "b"
                                                , Type.FunctionSignature
                                                    { input =
                                                        [ Type.Generic "a"
                                                        , Type.Generic "b"
                                                        ]
                                                    , output =
                                                        [ Type.Generic "b"
                                                        ]
                                                    }
                                                ]
                                                [ Type.Generic "b" ]
                                  , implementation =
                                        QAST.MultiImpl
                                            [ ( QAST.TypeMatch emptyRange (Type.Custom "Empty") []
                                              , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            , ( QAST.TypeMatch emptyRange (Type.CustomGeneric "NonEmpty" [ Type.Generic "a" ]) []
                                              , [ QAST.Function emptyRange ">Pair"
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.FunctionRef emptyRange "fold_quot1"
                                                , QAST.FunctionRef emptyRange "fold_quot2"
                                                , QAST.Function emptyRange "split"
                                                , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Builtin emptyRange Builtin.StackDuplicate
                                                , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                , QAST.Function emptyRange "spill"
                                                , QAST.Builtin emptyRange Builtin.Apply
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Function emptyRange "second>"
                                                , QAST.Function emptyRange "fold"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "fold_quot1"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isInline
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "head>" ]
                                  }
                                , { name = "fold_quot2"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isInline
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "rest>" ]
                                  }
                                , { name = "split"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a"
                                                , Type.FunctionSignature
                                                    { input = [ Type.Generic "a" ]
                                                    , output = [ Type.Generic "b" ]
                                                    }
                                                , Type.FunctionSignature
                                                    { input = [ Type.Generic "a" ]
                                                    , output = [ Type.Generic "c" ]
                                                    }
                                                ]
                                                [ Type.Generic "b"
                                                , Type.Generic "c"
                                                ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackLeftRotate
                                            , QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                            , QAST.Builtin emptyRange Builtin.Apply
                                            , QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                            , QAST.Builtin emptyRange Builtin.Apply
                                            , QAST.Builtin emptyRange Builtin.StackSwap
                                            ]
                                  }
                                , { name = "spill"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.CustomGeneric "Pair"
                                                    [ Type.Generic "a"
                                                    , Type.Generic "b"
                                                    ]
                                                ]
                                                [ Type.Generic "a"
                                                , Type.Generic "b"
                                                ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.FunctionRef emptyRange "spill_quot1"
                                            , QAST.FunctionRef emptyRange "spill_quot2"
                                            , QAST.Function emptyRange "split"
                                            ]
                                  }
                                , { name = "spill_quot1"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isInline
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "first>" ]
                                  }
                                , { name = "spill_quot2"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isInline
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "second>" ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Generic custom type fails if not generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box"
                                    True
                                    emptyRange
                                    []
                                    [ ( "element", Type.Generic "a" ) ]
                                ]
                        , functions =
                            Dict.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        , test "Generic custom type fails if wrong generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box"
                                    True
                                    emptyRange
                                    [ "a" ]
                                    [ ( "element", Type.Generic "b" ) ]
                                ]
                        , functions =
                            Dict.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        , describe "Quotations"
            [ test "Simple example" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.FunctionRef emptyRange "main__quot2"
                                                , QAST.Function emptyRange "apply-to-num"
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectTypeCheck input
            , test "With type annotation" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Int
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Int ]
                                                        , output = [ Type.Int ]
                                                        }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.FunctionRef emptyRange "main__quot2"
                                                , QAST.Function emptyRange "apply-to-num"
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectTypeCheck input
            , test "Typechecking involving a multi-arity quotation is fine _if_ arity info is in type annotation" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-nums"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.FunctionSignature
                                                        { input = [ Type.Int, Type.Int ]
                                                        , output = [ Type.Int ]
                                                        }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Integer emptyRange 2
                                                , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                                , QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "apply-to-nums"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectTypeCheck input
            , test "With generics" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "map"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Generic "a"
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Generic "a" ]
                                                        , output = [ Type.Generic "b" ]
                                                        }
                                                    ]
                                                    [ Type.Generic "b" ]
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectTypeCheck input
            , test "Within multiwords" <|
                \_ ->
                    let
                        maybeUnion genericName =
                            Type.Union (Just "Maybe")
                                [ Type.Generic genericName
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
                                    [ { name = "map"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ maybeUnion "a"
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Generic "a" ]
                                                        , output = [ Type.Generic "b" ]
                                                        }
                                                    ]
                                                    [ maybeUnion "b" ]
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch emptyRange (Type.Generic "a") []
                                                  , [ QAST.Builtin emptyRange Builtin.Apply
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch emptyRange (Type.Custom "Nil") []
                                                  , [ QAST.Builtin emptyRange Builtin.StackDrop
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
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isInline
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    expectTypeCheck input
            ]
        , describe "Recursive word definitions"
            [ test "With type annotation" <|
                \_ ->
                    let
                        listUnion =
                            Type.Union (Just "List")
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
                                    [ { name = "sum"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 0
                                                , QAST.Function emptyRange "sum-helper"
                                                ]
                                      }
                                    , { name = "sum-helper"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ listUnion, Type.Int ] [ Type.Int ]
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch emptyRange (Type.CustomGeneric "NonEmptyList" [ Type.Int ]) []
                                                  , [ QAST.Builtin emptyRange Builtin.StackSwap
                                                    , QAST.Builtin emptyRange Builtin.StackDuplicate
                                                    , QAST.Function emptyRange "first>"
                                                    , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                    , QAST.Function emptyRange "rest>"
                                                    , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                    , QAST.Builtin emptyRange Builtin.Plus
                                                    , QAST.Function emptyRange "sum-helper"
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
                                                , QAST.Integer emptyRange 2
                                                , QAST.Integer emptyRange 3
                                                , QAST.Function emptyRange "EmptyList"
                                                , QAST.Function emptyRange ">NonEmptyList"
                                                , QAST.Function emptyRange ">NonEmptyList"
                                                , QAST.Function emptyRange ">NonEmptyList"
                                                , QAST.Function emptyRange "sum"
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    expectTypeCheck input
            ]
        , test "Correct node types" <|
            \_ ->
                let
                    input =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Function emptyRange "drop-first"
                                            ]
                                  }
                                , { name = "drop-first"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "b" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        }

                    expectedResult =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , type_ =
                                        { input = []
                                        , output = [ Type.Int ]
                                        }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ IntLiteral emptyRange 1
                                            , IntLiteral emptyRange 2
                                            , Word emptyRange
                                                "drop-first"
                                                { input = [ Type.Int, Type.Int ]
                                                , output = [ Type.Int ]
                                                }
                                            ]
                                  }
                                , { name = "drop-first"
                                  , type_ =
                                        { input = [ Type.Generic "a", Type.Generic "b" ]
                                        , output = [ Type.Generic "b" ]
                                        }
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "b" ]
                                  , implementation =
                                        SoloImpl
                                            [ Builtin emptyRange Builtin.StackSwap
                                            , Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        }
                in
                expectAst input expectedResult
        ]
