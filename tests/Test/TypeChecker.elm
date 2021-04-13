module Test.TypeChecker exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Builtin as Builtin
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type
import Play.Qualifier as QAST
import Play.TypeChecker exposing (..)
import Play.TypeChecker.Problem as Problem
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as QualifierUtil


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
                                            , QAST.Word emptyRange "inc"
                                            , QAST.Word emptyRange "inc"
                                            , QAST.Word emptyRange "dec"
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
                case run input of
                    Err _ ->
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
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                in
                case run input of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Did not expect type check to succeed."
        , test "Custom data structure without fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "True" emptyRange [] []
                                ]
                        , words =
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
                                            [ QAST.Word emptyRange "True"
                                            , QAST.Word emptyRange "as-int"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                case run source of
                    Err _ ->
                        Expect.fail "Did not expect type check to fail"

                    Ok _ ->
                        Expect.pass
        , test "Custom data structure with fields" <|
            \_ ->
                let
                    source =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Person" emptyRange [] [ ( "age", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc-age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Custom "Person" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Word emptyRange "age>"
                                            , QAST.Integer emptyRange 1
                                            , QAST.Builtin emptyRange Builtin.Plus
                                            , QAST.Word emptyRange ">Person"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Word emptyRange ">Person"
                                            , QAST.Word emptyRange "inc-age"
                                            , QAST.Word emptyRange "age>"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                case run source of
                    Err _ ->
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
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Word emptyRange "over"
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
                case run input of
                    Err _ ->
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
                                            [ QAST.Integer emptyRange 5
                                            , QAST.Word emptyRange "square"
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
                case run input of
                    Err _ ->
                        Expect.fail "Did not expect type check to fail."

                    Ok _ ->
                        Expect.pass
        , test "Generic custom type" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box"
                                    emptyRange
                                    [ "a" ]
                                    [ ( "element", Type.Generic "a" ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 5
                                            , QAST.Word emptyRange ">Box"
                                            , QAST.Word emptyRange "element>"
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
                case run input of
                    Err _ ->
                        Expect.fail "Did not expect type check to fail."

                    Ok _ ->
                        Expect.pass
        , test "Generic types with rotations and quotations" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Coordinate"
                                    emptyRange
                                    []
                                    [ ( "x", Type.Int )
                                    , ( "y", Type.Int )
                                    ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Integer emptyRange 1
                                            , QAST.Integer emptyRange 2
                                            , QAST.Word emptyRange ">Coordinate"
                                            , QAST.WordRef emptyRange "main__quot1"
                                            , QAST.Word emptyRange "update-x"
                                            , QAST.Word emptyRange "x>"
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
                                                , Type.Quotation { input = [ Type.Int ], output = [ Type.Int ] }
                                                ]
                                                [ Type.Custom "Coordinate" ]
                                  , implementation =
                                        QAST.SoloImpl
                                            [ QAST.Builtin emptyRange Builtin.StackSwap
                                            , QAST.Builtin emptyRange Builtin.StackDuplicate
                                            , QAST.Word emptyRange "x>"
                                            , QAST.Builtin emptyRange Builtin.StackLeftRotate
                                            , QAST.Builtin emptyRange Builtin.Apply
                                            , QAST.Word emptyRange ">x"
                                            ]
                                  }
                                ]
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                case run input of
                    Err err ->
                        err
                            |> List.map (Problem.toString "")
                            |> String.join "\n"
                            |> (++) "Did not expect type check to fail.\n"
                            |> Expect.fail

                    Ok _ ->
                        Expect.pass
        , test "Generic custom type fails if not generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box" emptyRange [] [ ( "element", Type.Generic "a" ) ]
                                ]
                        , words =
                            Dict.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                case run input of
                    Ok _ ->
                        Expect.fail "Expected type check to fail."

                    Err _ ->
                        Expect.pass
        , test "Generic custom type fails if wrong generic is listed" <|
            \_ ->
                let
                    input =
                        { types =
                            Dict.fromListBy QAST.typeDefinitionName
                                [ QAST.CustomTypeDef "Box" emptyRange [ "a" ] [ ( "element", Type.Generic "b" ) ]
                                ]
                        , words =
                            Dict.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                case run input of
                    Ok _ ->
                        Expect.fail "Expected type check to fail."

                    Err _ ->
                        Expect.pass
        , describe "Unions and multifunctions" <|
            let
                boolUnion =
                    Type.Union
                        [ Type.Custom "True"
                        , Type.Custom "False"
                        ]

                template multiFn =
                    { types =
                        Dict.fromListBy QAST.typeDefinitionName
                            [ QAST.UnionTypeDef "Bool"
                                emptyRange
                                []
                                [ Type.Custom "True"
                                , Type.Custom "False"
                                ]
                            , QAST.CustomTypeDef "True" emptyRange [] []
                            , QAST.CustomTypeDef "False" emptyRange [] []
                            ]
                    , words =
                        Dict.fromListBy .name
                            [ multiFn
                            , { name = "main"
                              , metadata =
                                    Metadata.default
                                        |> Metadata.asEntryPoint
                              , implementation =
                                    QAST.SoloImpl
                                        [ QAST.Word emptyRange "True"
                                        , QAST.Word emptyRange "to-int"
                                        , QAST.Word emptyRange "False"
                                        , QAST.Word emptyRange "to-int"
                                        , QAST.Builtin emptyRange Builtin.Equal
                                        ]
                              }
                            ]
                    }
                        |> QualifierUtil.addFunctionsForStructs
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
                    case run input of
                        Err _ ->
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
                    case run input of
                        Err _ ->
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
                    case run input of
                        Err err ->
                            Expect.fail <| "Did not expect type check to fail." ++ Debug.toString err

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
                    case run input of
                        Err _ ->
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
                                        emptyRange
                                        []
                                        [ Type.Custom "Person"
                                        , Type.Custom "Dog"
                                        ]
                                    , QAST.CustomTypeDef "Person"
                                        emptyRange
                                        []
                                        [ ( "age", Type.Int ) ]
                                    , QAST.CustomTypeDef "Dog"
                                        emptyRange
                                        []
                                        [ ( "man-years", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "add-to-age"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch emptyRange (Type.Custom "Person") []
                                                  , [ QAST.Word emptyRange ">age"
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch emptyRange (Type.Custom "Dog") []
                                                  , [ QAST.Integer emptyRange 4
                                                    , QAST.Builtin emptyRange Builtin.Multiply
                                                    , QAST.Word emptyRange ">man-years"
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
                                                  , [ QAST.Word emptyRange "age>" ]
                                                  )
                                                , ( QAST.TypeMatch emptyRange (Type.Custom "Dog") []
                                                  , [ QAST.Word emptyRange "man-years>" ]
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
                                                , QAST.Word emptyRange ">Person"
                                                , QAST.Integer emptyRange 10
                                                , QAST.Word emptyRange "add-to-age"
                                                , QAST.Integer emptyRange 0
                                                , QAST.Word emptyRange ">Dog"
                                                , QAST.Integer emptyRange 2
                                                , QAST.Word emptyRange "add-to-age"
                                                , QAST.Word emptyRange "get-man-age"
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Word emptyRange "get-man-age"
                                                , QAST.Builtin emptyRange Builtin.StackSwap
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    case run input of
                        Err _ ->
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
                                        emptyRange
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , QAST.CustomTypeDef "True" emptyRange [] []
                                    , QAST.CustomTypeDef "False" emptyRange [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "not"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.MultiImpl
                                                [ ( QAST.TypeMatch emptyRange (Type.Custom "True") []
                                                  , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                    , QAST.Word emptyRange "False"
                                                    ]
                                                  )
                                                , ( QAST.TypeMatch emptyRange (Type.Custom "False") []
                                                  , [ QAST.Builtin emptyRange Builtin.StackDrop
                                                    , QAST.Word emptyRange "True"
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
                                                [ QAST.Word emptyRange "True"
                                                , QAST.Word emptyRange "not"
                                                , QAST.Word emptyRange "true-to-int"
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    case run input of
                        Ok _ ->
                            Expect.fail "Did not expect type check to pass."

                        Err _ ->
                            Expect.pass
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
                                        emptyRange
                                        [ "a" ]
                                        [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                                        , Type.Custom "EmptyList"
                                        ]
                                    , QAST.CustomTypeDef "NonEmptyList"
                                        emptyRange
                                        [ "a" ]
                                        [ ( "first", Type.Generic "a" )
                                        , ( "rest", listUnion )
                                        ]
                                    , QAST.CustomTypeDef "EmptyList" emptyRange [] []
                                    ]
                            , words =
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
                                                    , QAST.Word emptyRange "first>"
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
                                                , QAST.Word emptyRange "EmptyList"
                                                , QAST.Word emptyRange ">NonEmptyList"
                                                , QAST.Integer emptyRange 0
                                                , QAST.Word emptyRange "first-or-default"
                                                , QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Equal
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "Expected type check to pass."
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
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nil"
                                        ]
                                    , QAST.CustomTypeDef "Nil" emptyRange [] []
                                    ]
                            , words =
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
                                                [ QAST.Word emptyRange "Nil"
                                                , QAST.Integer emptyRange 1
                                                , QAST.Word emptyRange "with-default"
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
                    case run input of
                        Ok typedAst ->
                            Expect.equal expectedResult typedAst

                        Err errs ->
                            Expect.fail <| "Expected type check to pass, failed with: " ++ Debug.toString errs
            , test "Generic union fails if not generic is listed" <|
                \_ ->
                    let
                        input =
                            { types =
                                Dict.fromListBy QAST.typeDefinitionName
                                    [ QAST.UnionTypeDef "Maybe"
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "b"
                                        , Type.Custom "Nothing"
                                        ]
                                    , QAST.CustomTypeDef "Nothing" emptyRange [] []
                                    ]
                            , words =
                                Dict.empty
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    case run input of
                        Ok _ ->
                            Expect.fail "Expected type check to fail."

                        Err _ ->
                            Expect.pass
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
                                                , QAST.WordRef emptyRange "main__quot2"
                                                , QAST.Word emptyRange "apply-to-num"
                                                , QAST.WordRef emptyRange "main__quot1"
                                                , QAST.Word emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
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
                                                , QAST.WordRef emptyRange "main__quot2"
                                                , QAST.Word emptyRange "apply-to-num"
                                                , QAST.WordRef emptyRange "main__quot1"
                                                , QAST.Word emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quot2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "Did not expect type check to fail."
            , test "Typechecking involving a multi-arity quotation is fine _if_ arity info is in type annotation" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-nums"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Quotation
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
                                                [ QAST.WordRef emptyRange "main__quot1"
                                                , QAST.Word emptyRange "apply-to-nums"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail <| "Did not expect type check to fail:" ++ Debug.toString err
            , test "With generics" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "map"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Generic "a"
                                                    , Type.Quotation
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
                                                , QAST.WordRef emptyRange "main__quot1"
                                                , QAST.Word emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail <| "Did not expect type check to fail: " ++ Debug.toString err
            , test "Within multiwords" <|
                \_ ->
                    let
                        maybeUnion genericName =
                            Type.Union
                                [ Type.Generic genericName
                                , Type.Custom "Nil"
                                ]

                        input =
                            { types =
                                Dict.fromListBy QAST.typeDefinitionName
                                    [ QAST.UnionTypeDef "Maybe"
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nil"
                                        ]
                                    , QAST.CustomTypeDef "Nil" emptyRange [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "map"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ maybeUnion "a"
                                                    , Type.Quotation
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
                                                [ QAST.Word emptyRange "Nil"
                                                , QAST.WordRef emptyRange "main__quot1"
                                                , QAST.Word emptyRange "map"
                                                ]
                                      }
                                    , { name = "main__quot1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
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
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail <| "Did not expect type check to fail: " ++ Debug.toString err
            ]
        , describe "Recursive word definitions"
            [ test "With type annotation" <|
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
                                        emptyRange
                                        [ "a" ]
                                        [ Type.CustomGeneric "NonEmptyList" [ Type.Generic "a" ]
                                        , Type.Custom "EmptyList"
                                        ]
                                    , QAST.CustomTypeDef "NonEmptyList"
                                        emptyRange
                                        [ "a" ]
                                        [ ( "first", Type.Generic "a" )
                                        , ( "rest", listUnion )
                                        ]
                                    , QAST.CustomTypeDef "EmptyList" emptyRange [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "sum"
                                      , metadata = Metadata.default
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 0
                                                , QAST.Word emptyRange "sum-helper"
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
                                                    , QAST.Word emptyRange "first>"
                                                    , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                    , QAST.Word emptyRange "rest>"
                                                    , QAST.Builtin emptyRange Builtin.StackRightRotate
                                                    , QAST.Builtin emptyRange Builtin.Plus
                                                    , QAST.Word emptyRange "sum-helper"
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
                                                , QAST.Word emptyRange "EmptyList"
                                                , QAST.Word emptyRange ">NonEmptyList"
                                                , QAST.Word emptyRange ">NonEmptyList"
                                                , QAST.Word emptyRange ">NonEmptyList"
                                                , QAST.Word emptyRange "sum"
                                                ]
                                      }
                                    ]
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    case run input of
                        Ok _ ->
                            Expect.pass

                        Err _ ->
                            Expect.fail "Did not expect type check to fail."
            ]
        , describe "Correct node types"
            [ test "Simple case" <|
                \_ ->
                    let
                        input =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            QAST.SoloImpl
                                                [ QAST.Integer emptyRange 1
                                                , QAST.Integer emptyRange 2
                                                , QAST.Word emptyRange "drop-first"
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
                    case run input of
                        Ok result ->
                            Expect.equal expectedResult result

                        Err errs ->
                            Expect.fail <| "Did not expect type check to fail: " ++ Debug.toString errs
            ]
        ]
