module Test.Qualifier.ModuleResolution exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.SourceLocation as PSourceLoc
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem as Problem
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as QualifierUtil


suite : Test
suite =
    describe "Qualifier -- Module resolution"
        [ test "When module does have a definition, only functions defined to be exposed are" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports = Dict.empty
                                , exposes = Set.fromList [ "fn2" ]
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "fn1"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer PSourceLoc.emptyRange 1
                                            , AST.Integer PSourceLoc.emptyRange 2
                                            , AST.Function PSourceLoc.emptyRange "+"
                                            ]
                                  }
                                , { name = "fn2"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer PSourceLoc.emptyRange 2
                                            , AST.Integer PSourceLoc.emptyRange 3
                                            , AST.Function PSourceLoc.emptyRange "+"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "fn1"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "fn2"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            , Integer emptyRange 3
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectOutput unqualifiedAst expectedAst
        , test "Referencing a function from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports =
                                    Dict.fromList
                                        [ ( "internal/mod", [] )
                                        ]
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer PSourceLoc.emptyRange 1
                                            , AST.Function PSourceLoc.emptyRange "value"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWordUnexposed "internal/mod/value"
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/mod", "external/package" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed function is called"

                    Err [ Problem.FunctionNotExposed _ "internal/mod/value" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a function from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports =
                                    Dict.fromList
                                        [ ( "/mod", [ "add" ] )
                                        ]
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer PSourceLoc.emptyRange 1
                                            , AST.Function PSourceLoc.emptyRange "add"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWordUnexposed "/external/package/mod/add"
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/mod", "external/package" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed function is called"

                    Err [ Problem.FunctionNotExposed _ "/external/package/mod/add" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type signature from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.ExternalRef [ "mod" ] "Tipe" [] ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function PSourceLoc.emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "/external/package/mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/mod", "external/package" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "/external/package/mod/Tipe" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type definition from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "BoxedTipe"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "value", AST.ExternalRef [ "mod" ] "TipeUnion" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "/external/package/mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/mod", "external/package" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "/external/package/mod/TipeUnion" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type match from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch PSourceLoc.emptyRange (AST.ExternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function PSourceLoc.emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function PSourceLoc.emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "/external/package/mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/mod", "external/package" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "/external/package/mod/Tipe" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type signature from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "mod" ] "Tipe" [] ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function PSourceLoc.emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules = Dict.empty
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "mod/Tipe" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type definition from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "BoxedTipe"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "value", AST.InternalRef [ "mod" ] "TipeUnion" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules = Dict.empty
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "mod/TipeUnion" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "Referencing a type in a type match from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch PSourceLoc.emptyRange (AST.InternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function PSourceLoc.emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function PSourceLoc.emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types =
                            Dict.fromList <|
                                dummyTypeUnexposed "mod/Tipe"
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules = Dict.empty
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected qualification to fail because an unexposed type is referenced"

                    Err [ Problem.TypeNotExposed _ "mod/Tipe" ] ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        ]
