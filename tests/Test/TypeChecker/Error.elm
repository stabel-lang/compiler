module Test.TypeChecker.Error exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Play.Data.Builtin as Builtin
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type
import Play.Qualifier exposing (..)
import Play.TypeChecker as TypeChecker
import Play.TypeChecker.Problem as Problem
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TypeChecker errors"
        [ test "Undeclared generic" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ CustomTypeDef "Box"
                                    emptyRange
                                    [ "a" ]
                                    [ ( "value", Type.Generic "b" ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            ]
                                  }
                                ]
                        }

                    undeclaredGenericError generic problem =
                        case problem of
                            Problem.UndeclaredGeneric _ problemGeneric _ ->
                                generic == problemGeneric

                            _ ->
                                False
                in
                checkForError (undeclaredGenericError "b") ast
        , test "Wrong type signature" <|
            \_ ->
                let
                    ast =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Integer emptyRange 2
                                            ]
                                  }
                                ]
                        }

                    undeclaredGenericError problem =
                        case problem of
                            Problem.TypeError _ "main" provided inferred ->
                                (provided == { input = [], output = [ Type.Int ] })
                                    && (inferred == { input = [], output = [ Type.Int, Type.Int ] })

                            _ ->
                                False
                in
                checkForError undeclaredGenericError ast
        , test "Unexpected function" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ CustomTypeDef "IntBox"
                                    emptyRange
                                    []
                                    [ ( "value", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Word emptyRange "value>"
                                            ]
                                  }
                                , { name = ">IntBox"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "IntBox" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "IntBox" ]
                                  }
                                , { name = ">value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "IntBox", Type.Int ] [ Type.Custom "IntBox" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "IntBox" "value" ]
                                  }
                                , { name = "value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "IntBox" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "IntBox" "value" ]
                                  }
                                ]
                        }

                    undeclaredGenericError problem =
                        case problem of
                            Problem.UnexpectedType _ "main" (Type.Custom "IntBox") Type.Int ->
                                True

                            _ ->
                                False
                in
                checkForError undeclaredGenericError ast
        , test "An inferred concrete output type should not successfully type check against a generic variable" <|
            \_ ->
                let
                    ast =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Generic "in" ] [ Type.Generic "out" ]
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                ]
                        }

                    typeError problem =
                        case problem of
                            Problem.TypeError _ "main" _ _ ->
                                True

                            _ ->
                                False
                in
                checkForError typeError ast
        , test "An inferred union output type should not successfully type check against a generic variable" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    emptyRange
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" emptyRange [] []
                                , CustomTypeDef "False" emptyRange [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Generic "out" ]
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 0
                                            , Word emptyRange "true-or-false"
                                            ]
                                  }
                                , { name = "true-or-false"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Int ]
                                                [ Type.Union [ Type.Generic "a", Type.Generic "b" ] ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange Type.Int [ ( "value", LiteralInt 0 ) ]
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Word emptyRange ">False"
                                                ]
                                              )
                                            , ( TypeMatch emptyRange Type.Int []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Word emptyRange ">True"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = ">True"
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
                                ]
                        }
                in
                case TypeChecker.run ast of
                    Err errors ->
                        Expect.equalLists
                            [ Problem.TypeError emptyRange
                                "main"
                                { input = [], output = [ Type.Generic "a" ] }
                                { input = [], output = [ Type.Union [ Type.Generic "b", Type.Generic "a" ] ] }
                            , Problem.TypeError emptyRange
                                "true-or-false"
                                { input = [ Type.Int ], output = [ Type.Union [ Type.Generic "b", Type.Generic "a" ] ] }
                                { input = [ Type.Int ], output = [ Type.Union [ Type.Custom "False", Type.Custom "True" ] ] }
                            ]
                            errors

                    Ok _ ->
                        Expect.fail "Did not expect type checking to succeed"
        , describe "Inexhaustiveness checking"
            [ test "Simple example" <|
                \_ ->
                    let
                        ast =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Word emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ]
                                                  , [ Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiWord _ [ [ Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError ast
            , test "Default clause is exhaustive" <|
                \_ ->
                    let
                        ast =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Word emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ]
                                                  , [ Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                ]
                                                [ Integer emptyRange 0
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case TypeChecker.run ast of
                        Ok _ ->
                            Expect.pass

                        Err errs ->
                            Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errs
            , test "Nested" <|
                \_ ->
                    let
                        ast =
                            { types =
                                Dict.fromListBy typeDefinitionName
                                    [ CustomTypeDef "IntBox"
                                        emptyRange
                                        []
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                                |> Metadata.withType [] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange ">IntBox"
                                                , Word emptyRange "mword"
                                                , Word emptyRange "value>"
                                                ]
                                      }
                                    , { name = "mword"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (Type.Custom "IntBox")
                                                        [ ( "value>"
                                                          , RecursiveMatch
                                                                (TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ])
                                                          )
                                                        ]
                                                  , [ Word emptyRange "value>"
                                                    , Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    , Word emptyRange ">IntBox"
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = ">IntBox"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "IntBox" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "IntBox" ]
                                      }
                                    , { name = ">value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Custom "IntBox", Type.Int ] [ Type.Custom "IntBox" ]
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "IntBox" "value" ]
                                      }
                                    , { name = "value>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Custom "IntBox" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "IntBox" "value" ]
                                      }
                                    ]
                            }

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiWord _ [ [ Type.Custom "IntBox", Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError ast
            , test "A total branch should remove any earlier seen branch" <|
                \_ ->
                    let
                        ast =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Word emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ]
                                                  , [ Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                , ( TypeMatch emptyRange Type.Int []
                                                  , [ Builtin emptyRange Builtin.StackDuplicate
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case TypeChecker.run ast of
                        Err errors ->
                            Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

                        Ok _ ->
                            Expect.pass
            , test "A total branch should prevent addition of later partial branch" <|
                \_ ->
                    let
                        ast =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Word emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int []
                                                  , [ Builtin emptyRange Builtin.StackDuplicate
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                , ( TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ]
                                                  , [ Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case TypeChecker.run ast of
                        Err errors ->
                            Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

                        Ok _ ->
                            Expect.pass
            , test "Test with non-int type as pattern" <|
                \_ ->
                    let
                        maybeUnion =
                            Type.Union
                                [ Type.Custom "IntBox"
                                , Type.Custom "Nil"
                                ]

                        input =
                            { types =
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Maybe"
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef "IntBox"
                                        emptyRange
                                        []
                                        [ ( "value", Type.Int ) ]
                                    , CustomTypeDef "Nil" emptyRange [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Nil"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "Nil" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "Nil"
                                                ]
                                      }
                                    , { name = ">IntBox"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "IntBox" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "IntBox" ]
                                      }
                                    , { name = ">value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Custom "IntBox", Type.Int ] [ Type.Custom "IntBox" ]
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "IntBox" "value" ]
                                      }
                                    , { name = "value>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType [ Type.Custom "IntBox" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "IntBox" "value" ]
                                      }
                                    , { name = "with-default"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ maybeUnion, Type.Int ]
                                                    [ Type.Int ]
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (Type.Custom "IntBox")
                                                        [ ( "value>"
                                                          , RecursiveMatch <|
                                                                TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 0 ) ]
                                                          )
                                                        ]
                                                  , [ Builtin emptyRange Builtin.StackDrop
                                                    , Word emptyRange "value>"
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
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Word emptyRange ">Nil"
                                                , Integer emptyRange 1
                                                , Word emptyRange "with-default"
                                                ]
                                      }
                                    ]
                            }

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiWord _ [ [ Type.Custom "IntBox", Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError input
            ]
        ]


checkForError : (Problem.Problem -> Bool) -> AST -> Expectation
checkForError fn source =
    case TypeChecker.run source of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect type checking to succeed"
