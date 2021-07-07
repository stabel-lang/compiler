module Test.TypeChecker.Unions exposing (suite)

import Dict
import Dict.Extra as Dict
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type exposing (Type)
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
    describe "TypeChecker -- Unions and multifunctions"
        [ test "Simplest case" <|
            \_ ->
                let
                    input =
                        template
                            { name = "to-int"
                            , exposed = True
                            , sourceLocation = Nothing
                            , typeSignature = TypeSignature.NotProvided
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
                            , exposed = True
                            , sourceLocation = Nothing
                            , typeSignature =
                                TypeSignature.UserProvided
                                    { input = [ boolUnion ]
                                    , output = [ Type.Int ]
                                    }
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
                            , exposed = True
                            , sourceLocation = Nothing
                            , typeSignature =
                                TypeSignature.UserProvided
                                    { input = [ boolUnion ]
                                    , output = [ Type.Int ]
                                    }
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
                            , exposed = True
                            , sourceLocation = Nothing
                            , typeSignature = TypeSignature.NotProvided
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
                            Dict.fromListBy .name
                                [ { name = "Beings"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.UnionMembers
                                            [ Type.Custom "Person"
                                            , Type.Custom "Dog"
                                            ]
                                  }
                                , { name = "Person"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.StructMembers
                                            [ ( "age", Type.Int ) ]
                                  }
                                , { name = "Dog"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.StructMembers
                                            [ ( "man-years", Type.Int ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "add-to-age"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Function requiring a concrete type should not accept an union with that type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        QAST.UnionMembers
                                            [ Type.Custom "True"
                                            , Type.Custom "False"
                                            ]
                                  }
                                , { name = "True"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = QAST.StructMembers []
                                  }
                                , { name = "False"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = QAST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "not"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "True" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackDrop
                                            , QAST.Integer emptyRange 1
                                            ]
                                  }
                                , { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "True"
                                            , QAST.Function emptyRange "not"
                                            , QAST.Function emptyRange "true-to-int"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        , test "Generic union" <|
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
                                [ { name = "first-or-default"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ listUnion, Type.Generic "a" ]
                                            , output = [ Type.Generic "a" ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
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
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheck input
        , test "Union with generic branch" <|
            \_ ->
                let
                    maybeUnion =
                        Type.Union (Just "Maybe")
                            [ Type.Generic "a"
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
                                [ { name = "with-default"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ maybeUnion, Type.Generic "a" ]
                                            , output = [ Type.Generic "a" ]
                                            }
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
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Function emptyRange "Nil"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Function emptyRange "with-default"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs

                    expectedResult =
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
                              , metadata = Metadata.default
                              , implementation =
                                    SoloImpl
                                        [ Function emptyRange
                                            "Nil"
                                            { input = []
                                            , output = [ Type.Custom "Nil" ]
                                            }
                                        , IntLiteral emptyRange 1
                                        , Function emptyRange
                                            "with-default"
                                            { input =
                                                [ Type.Union (Just "Maybe")
                                                    [ Type.Int
                                                    , Type.Custom "Nil"
                                                    ]
                                                , Type.Int
                                                ]
                                            , output = [ Type.Int ]
                                            }
                                        ]
                              }
                            ]
                in
                expectAst input expectedResult
        , test "Generic union fails if not generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Maybe"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members =
                                        QAST.UnionMembers
                                            [ Type.Generic "b"
                                            , Type.Custom "Nothing"
                                            ]
                                  }
                                , { name = "Nothing"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = QAST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                expectTypeCheckFailure input
        ]


boolUnion : Type
boolUnion =
    Type.Union (Just "Bool")
        [ Type.Custom "True"
        , Type.Custom "False"
        ]


template : QAST.FunctionDefinition -> QAST.AST
template multiFn =
    { types =
        Dict.fromListBy .name
            [ { name = "Bool"
              , exposed = True
              , sourceLocation = emptyRange
              , generics = []
              , members =
                    QAST.UnionMembers
                        [ Type.Custom "True"
                        , Type.Custom "False"
                        ]
              }
            , { name = "True"
              , exposed = True
              , sourceLocation = emptyRange
              , generics = []
              , members = QAST.StructMembers []
              }
            , { name = "False"
              , exposed = True
              , sourceLocation = emptyRange
              , generics = []
              , members = QAST.StructMembers []
              }
            ]
    , functions =
        Dict.fromListBy .name
            [ multiFn
            , { name = "main"
              , exposed = True
              , sourceLocation = Nothing
              , typeSignature = TypeSignature.NotProvided
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
    , referenceableFunctions = Set.empty
    }
        |> QualifierUtil.addFunctionsForStructs
