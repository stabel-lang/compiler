module Test.TypeChecker.Errors exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Qualifier exposing (..)
import Stabel.TypeChecker as TypeChecker
import Stabel.TypeChecker.Problem as Problem
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TypeChecker -- Errors"
        [ test "Undeclared generic" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Box"
                                  , exposed = False
                                  , sourceLocation = emptyRange
                                  , generics = [ "a" ]
                                  , members = StructMembers [ ( "value", Type.Generic "b" ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Integer emptyRange 2
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                            Dict.fromListBy .name
                                [ { name = "IntBox"
                                  , exposed = False
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers [ ( "value", Type.Int ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Int ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "value>"
                                            ]
                                  }
                                , { name = ">IntBox"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Custom "IntBox" ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "IntBox" ]
                                  }
                                , { name = ">value"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "IntBox", Type.Int ]
                                            , output = [ Type.Custom "IntBox" ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "IntBox" "value" ]
                                  }
                                , { name = "value>"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "IntBox" ]
                                            , output = [ Type.Int ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "IntBox" "value" ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Generic "in" ]
                                            , output = [ Type.Generic "out" ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
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
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , exposed = False
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        UnionMembers
                                            [ Type.Custom "True"
                                            , Type.Custom "False"
                                            ]
                                  }
                                , { name = "True"
                                  , exposed = False
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                , { name = "False"
                                  , exposed = False
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = []
                                            , output = [ Type.Generic "out" ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 0
                                            , Function emptyRange "true-or-false"
                                            ]
                                  }
                                , { name = "true-or-false"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Int ]
                                            , output =
                                                [ Type.Union Nothing
                                                    [ Type.Generic "a"
                                                    , Type.Generic "b"
                                                    ]
                                                ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange Type.Int [ ( "value", LiteralInt 0 ) ]
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Function emptyRange ">False"
                                                ]
                                              )
                                            , ( TypeMatch emptyRange Type.Int []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Function emptyRange ">True"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = ">True"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = []
                                            , output =
                                                [ Type.Custom "True"
                                                ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = []
                                            , output =
                                                [ Type.Custom "False" ]
                                            }
                                  , exposed = False
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "False"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                case TypeChecker.run ast of
                    Err errors ->
                        Expect.equalLists
                            [ Problem.TypeError emptyRange
                                "main"
                                { input = [], output = [ Type.Generic "a" ] }
                                { input = []
                                , output =
                                    [ Type.Union Nothing
                                        [ Type.Generic "b", Type.Generic "a" ]
                                    ]
                                }
                            , Problem.TypeError emptyRange
                                "true-or-false"
                                { input = [ Type.Int ]
                                , output =
                                    [ Type.Union Nothing
                                        [ Type.Generic "b", Type.Generic "a" ]
                                    ]
                                }
                                { input = [ Type.Int ]
                                , output =
                                    [ Type.Union Nothing
                                        [ Type.Custom "False", Type.Custom "True" ]
                                    ]
                                }
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
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Function emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
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
                            , referenceableFunctions = Set.empty
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
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Function emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
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
                            , referenceableFunctions = Set.empty
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
                                Dict.fromListBy .name
                                    [ { name = "IntBox"
                                      , exposed = False
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            StructMembers
                                                [ ( "value", Type.Int ) ]
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.UserProvided
                                                { input = []
                                                , output = [ Type.Int ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Function emptyRange ">IntBox"
                                                , Function emptyRange "mword"
                                                , Function emptyRange "value>"
                                                ]
                                      }
                                    , { name = "mword"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (Type.Custom "IntBox")
                                                        [ ( "value>"
                                                          , RecursiveMatch
                                                                (TypeMatch emptyRange Type.Int [ ( "value>", LiteralInt 1 ) ])
                                                          )
                                                        ]
                                                  , [ Function emptyRange "value>"
                                                    , Integer emptyRange 1
                                                    , Builtin emptyRange Builtin.Plus
                                                    , Function emptyRange ">IntBox"
                                                    ]
                                                  )
                                                ]
                                                []
                                      }
                                    , { name = ">IntBox"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Int ]
                                                , output = [ Type.Custom "IntBox" ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "IntBox" ]
                                      }
                                    , { name = ">value"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Custom "IntBox", Type.Int ]
                                                , output = [ Type.Custom "IntBox" ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "IntBox" "value" ]
                                      }
                                    , { name = "value>"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Custom "IntBox" ]
                                                , output = [ Type.Int ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "IntBox" "value" ]
                                      }
                                    ]
                            , referenceableFunctions = Set.empty
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
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Function emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
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
                            , referenceableFunctions = Set.empty
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
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Function emptyRange "mword"
                                                ]
                                      }
                                    , { name = "mword"
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
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
                            , referenceableFunctions = Set.empty
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
                            Type.Union (Just "Maybe")
                                [ Type.Custom "IntBox"
                                , Type.Custom "Nil"
                                ]

                        input =
                            { types =
                                Dict.fromListBy .name
                                    [ { name = "Maybe"
                                      , exposed = False
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            UnionMembers
                                                [ Type.Generic "a"
                                                , Type.Custom "Nil"
                                                ]
                                      }
                                    , { name = "IntBox"
                                      , exposed = False
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            StructMembers
                                                [ ( "value", Type.Int ) ]
                                      }
                                    , { name = "Nil"
                                      , exposed = False
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = ">Nil"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = []
                                                , output = [ Type.Custom "Nil" ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "Nil"
                                                ]
                                      }
                                    , { name = ">IntBox"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Int ]
                                                , output = [ Type.Custom "IntBox" ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "IntBox" ]
                                      }
                                    , { name = ">value"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Custom "IntBox", Type.Int ]
                                                , output = [ Type.Custom "IntBox" ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "IntBox" "value" ]
                                      }
                                    , { name = "value>"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ Type.Custom "IntBox" ]
                                                , output = [ Type.Int ]
                                                }
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "IntBox" "value" ]
                                      }
                                    , { name = "with-default"
                                      , sourceLocation = Nothing
                                      , typeSignature =
                                            TypeSignature.CompilerProvided
                                                { input = [ maybeUnion, Type.Int ]
                                                , output = [ Type.Int ]
                                                }
                                      , exposed = False
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
                                                    , Function emptyRange "value>"
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
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , exposed = False
                                      , implementation =
                                            SoloImpl
                                                [ Function emptyRange ">Nil"
                                                , Integer emptyRange 1
                                                , Function emptyRange "with-default"
                                                ]
                                      }
                                    ]
                            , referenceableFunctions = Set.empty
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
