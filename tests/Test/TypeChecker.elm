module Test.TypeChecker exposing (suite)

import Dict
import Dict.Extra as Dict
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                        , referenceableFunctions = Set.empty
                        }

                    expectedResult =
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
                              , metadata = Metadata.default
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Int ]
                                            , output = []
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                expectTypeCheckFailure input
        , test "Custom data structure without fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "True"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = QAST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "as-int"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            ]
                                  }
                                , { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "True"
                                            , QAST.Function emptyRange "as-int"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck source
        , test "Custom data structure with fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Person"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.StructMembers
                                            [ ( "age", Type.Int ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc-age"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "Person" ]
                                            , output = [ Type.Custom "Person" ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "age>"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            , QAST.Function emptyRange ">Person"
                                            ]
                                  }
                                , { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange ">Person"
                                            , QAST.Function emptyRange "inc-age"
                                            , QAST.Function emptyRange "age>"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        -- Most would start at a and increment, but we need to check that
                                        -- the typechecker cares about the relationship between these generic
                                        -- variables, not the names themselves
                                        TypeSignature.UserProvided
                                            { input = [ Type.Generic "b", Type.Generic "c" ]
                                            , output = [ Type.Generic "b", Type.Generic "c", Type.Generic "b" ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Builtin emptyRange Builtin.StackRightRotate
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 5
                                            , QAST.Function emptyRange "square"
                                            ]
                                  }
                                , { name = "square"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Builtin emptyRange Builtin.Multiply
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                expectTypeCheck input
        , test "Generic custom type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Box"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members =
                                        QAST.StructMembers
                                            [ ( "element", Type.Generic "a" ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
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
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Generic types with rotations and quotations" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Coordinate"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.StructMembers
                                            [ ( "x", Type.Int )
                                            , ( "y", Type.Int )
                                            ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "update-x"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Custom "Coordinate"
                                                , Type.FunctionSignature { input = [ Type.Int ], output = [ Type.Int ] }
                                                ]
                                            , output = [ Type.Custom "Coordinate" ]
                                            }
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
                        , referenceableFunctions = Set.empty
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
                            Dict.fromListBy .name
                                [ { name = "Pair"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a", "b" ]
                                  , members =
                                        QAST.StructMembers
                                            [ ( "first", Type.Generic "a" )
                                            , ( "second", Type.Generic "b" )
                                            ]
                                  }
                                , { name = "List"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members = QAST.UnionMembers listUnion
                                  }
                                , { name = "NonEmpty"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members =
                                        QAST.StructMembers
                                            [ ( "head", Type.Generic "a" )
                                            , ( "rest", Type.Union (Just "List") listUnion )
                                            ]
                                  }
                                , { name = "Empty"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = QAST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "fold"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
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
                                            , output = [ Type.Generic "b" ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "head>" ]
                                  }
                                , { name = "fold_quot2"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "rest>" ]
                                  }
                                , { name = "split"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
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
                                            , output =
                                                [ Type.Generic "b"
                                                , Type.Generic "c"
                                                ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.CustomGeneric "Pair"
                                                    [ Type.Generic "a"
                                                    , Type.Generic "b"
                                                    ]
                                                ]
                                            , output =
                                                [ Type.Generic "a"
                                                , Type.Generic "b"
                                                ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.FunctionRef emptyRange "spill_quot1"
                                            , QAST.FunctionRef emptyRange "spill_quot2"
                                            , QAST.Function emptyRange "split"
                                            ]
                                  }
                                , { name = "spill_quot1"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "first>" ]
                                  }
                                , { name = "spill_quot2"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "second>" ]
                                  }
                                ]
                        , referenceableFunctions =
                            Set.fromList
                                [ "fold_quot1"
                                , "fold_quot2"
                                , "spill_quot1"
                                , "spill_quot2"
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
                            Dict.fromListBy .name
                                [ { name = "Box"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.StructMembers
                                            [ ( "element", Type.Generic "a" ) ]
                                  }
                                ]
                        , functions =
                            Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        , test "Generic custom type fails if wrong generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Box"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members =
                                        QAST.StructMembers
                                            [ ( "element", Type.Generic "b" ) ]
                                  }
                                ]
                        , functions =
                            Dict.empty
                        , referenceableFunctions = Set.empty
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "main__quot2"
                                    , "main__quot1"
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input =
                                                    [ Type.Int
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Int ]
                                                        , output = [ Type.Int ]
                                                        }
                                                    ]
                                                , output = [ Type.Int ]
                                                }
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "main__quot2"
                                    , "main__quot1"
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input =
                                                    [ Type.FunctionSignature
                                                        { input = [ Type.Int, Type.Int ]
                                                        , output = [ Type.Int ]
                                                        }
                                                    ]
                                                , output = [ Type.Int ]
                                                }
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Integer emptyRange 2
                                                , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                                , QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "apply-to-nums"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "main__quot1" ]
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input =
                                                    [ Type.Generic "a"
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Generic "a" ]
                                                        , output = [ Type.Generic "b" ]
                                                        }
                                                    ]
                                                , output = [ Type.Generic "b" ]
                                                }
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "main__quot1" ]
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
                                Dict.fromListBy .name
                                    [ { name = "Maybe"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            QAST.UnionMembers
                                                [ Type.Generic "a"
                                                , Type.Custom "Nil"
                                                ]
                                      }
                                    , { name = "Nil"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = QAST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "map"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input =
                                                    [ maybeUnion "a"
                                                    , Type.FunctionSignature
                                                        { input = [ Type.Generic "a" ]
                                                        , output = [ Type.Generic "b" ]
                                                        }
                                                    ]
                                                , output = [ maybeUnion "b" ]
                                                }
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Function emptyRange "Nil"
                                                , QAST.FunctionRef emptyRange "main__quot1"
                                                , QAST.Function emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "main__quot1" ]
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
                                Dict.fromListBy .name
                                    [ { name = "List"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            QAST.UnionMembers
                                                [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                                                , Type.Custom "EmptyList"
                                                ]
                                      }
                                    , { name = "NonEmptyList"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            QAST.StructMembers
                                                [ ( "first", Type.Generic "a" )
                                                , ( "rest", listUnion )
                                                ]
                                      }
                                    , { name = "EmptyList"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = QAST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "sum"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 0
                                                , QAST.Function emptyRange "sum-helper"
                                                ]
                                      }
                                    , { name = "sum-helper"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input = [ listUnion, Type.Int ]
                                                , output = [ Type.Int ]
                                                }
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
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
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
                            , referenceableFunctions = Set.empty
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Function emptyRange "drop-first"
                                            ]
                                  }
                                , { name = "drop-first"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Generic "a", Type.Generic "b" ]
                                            , output = [ Type.Generic "b" ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    expectedResult =
                        Dict.fromListBy .name
                            [ { name = "main"
                              , type_ =
                                    { input = []
                                    , output = [ Type.Int ]
                                    }
                              , metadata = Metadata.default
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
                in
                expectAst input expectedResult
        ]
