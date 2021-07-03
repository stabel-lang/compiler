module Test.Qualifier.ModuleResolution exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem as Problem
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as QualifierUtil


suite : Test
suite =
    describe "Qualifier -- Module resolution"
        [ test "Qualifies word in internal package" <|
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
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.PackageFunction emptyRange [ "internal" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "internal/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "internal/add" ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies word in internal package (multiword)" <|
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
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) []
                                              , [ AST.Integer emptyRange 0
                                                , AST.PackageFunction emptyRange [ "mod" ] "add"
                                                ]
                                              )
                                            ]
                                            [ AST.Integer emptyRange 1
                                            , AST.PackageFunction emptyRange [ "mod" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange Type.Int []
                                              , [ Integer emptyRange 0
                                                , Function emptyRange "mod/add"
                                                ]
                                              )
                                            ]
                                            [ Integer emptyRange 1
                                            , Function emptyRange "mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "mod/add" ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies type in internal package" <|
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
                                            { input =
                                                [ AST.NotStackRange <| AST.InternalRef [ "mod" ] "Tipe" []
                                                , AST.NotStackRange <| AST.InternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]
                                                , AST.NotStackRange <| AST.InternalRef [ "mod" ] "TipeUnion" []
                                                ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Custom "mod/Tipe"
                                                , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                , Type.Union (Just "mod/TipeUnion")
                                                    [ Type.Custom "mod/Tipe"
                                                    , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                    ]
                                                ]
                                            , output = []
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies type in internal package (multiword)" <|
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
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "mod" ] "TipeUnion" [] ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.InternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Function emptyRange "drop"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.InternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]) []
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Function emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Union (Just "mod/TipeUnion")
                                                    [ Type.Custom "mod/Tipe"
                                                    , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                    ]
                                                ]
                                            , output = []
                                            }
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "mod/Tipe") []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]) []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            ]
                                            [ Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies member type in internal package" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "SomeType"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "tipe", AST.InternalRef [ "mod" ] "Tipe" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "SomeType"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers [ ( "tipe", Type.Custom "mod/Tipe" ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies word in external package" <|
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
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.ExternalFunction emptyRange [ "mod" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add" ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies word in external package (multiword)" <|
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
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) []
                                              , [ AST.Integer emptyRange 0
                                                , AST.ExternalFunction emptyRange [ "mod" ] "add"
                                                ]
                                              )
                                            ]
                                            [ AST.Integer emptyRange 1
                                            , AST.ExternalFunction emptyRange [ "mod" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange Type.Int []
                                              , [ Integer emptyRange 0
                                                , Function emptyRange "/external/package/mod/add"
                                                ]
                                              )
                                            ]
                                            [ Integer emptyRange 1
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add" ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies type in external package" <|
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
                                            { input =
                                                [ AST.NotStackRange <| AST.ExternalRef [ "mod" ] "Tipe" []
                                                , AST.NotStackRange <| AST.ExternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]
                                                , AST.NotStackRange <| AST.ExternalRef [ "mod" ] "TipeUnion" []
                                                ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Custom "/external/package/mod/Tipe"
                                                , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                , Type.Union (Just "/external/package/mod/TipeUnion")
                                                    [ Type.Custom "/external/package/mod/Tipe"
                                                    , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                    ]
                                                ]
                                            , output = []
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies type in external package (multiword)" <|
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
                                            { input = [ AST.NotStackRange <| AST.ExternalRef [ "mod" ] "TipeUnion" [] ]
                                            , output = []
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.ExternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Function emptyRange "drop"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.ExternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]) []
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Function emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
                                            , AST.Function emptyRange "drop"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Union (Just "/external/package/mod/TipeUnion")
                                                    [ Type.Custom "/external/package/mod/Tipe"
                                                    , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                    ]
                                                ]
                                            , output = []
                                            }
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/external/package/mod/Tipe") []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]) []
                                              , [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                              )
                                            ]
                                            [ Builtin emptyRange Builtin.StackDrop
                                            , Builtin emptyRange Builtin.StackDrop
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Qualifies member type in external package" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "SomeType"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "tipe", AST.ExternalRef [ "mod" ] "Tipe" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "SomeType"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "tipe", Type.Custom "/external/package/mod/Tipe" ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe" ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module alias" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "ext", "/mod" )
                                        , ( "internal", "internal/mod" )
                                        ]
                                , imports = Dict.empty
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "ext" ] "Tipe" [] ]
                                            , output = [ AST.NotStackRange <| AST.InternalRef [ "internal" ] "Tope" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.PackageFunction emptyRange [ "internal" ] "value"
                                            , AST.PackageFunction emptyRange [ "ext" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input =
                                                [ Type.Custom "/external/package/mod/Tipe"
                                                ]
                                            , output = [ Type.Custom "internal/mod/Tope" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "internal/mod/value"
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add"
                                , dummyWord "internal/mod/value"
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module and function aliases" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "ext", "/mod" ) ]
                                , imports = Dict.empty
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "ext" ] "Tipe" [] ]
                                            , output = [ AST.NotStackRange <| AST.InternalRef [ "internal" ] "Tope" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "internal", "internal/mod" ) ]
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.PackageFunction emptyRange [ "internal" ] "value"
                                            , AST.PackageFunction emptyRange [ "ext" ] "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "/external/package/mod/Tipe" ]
                                            , output = [ Type.Custom "internal/mod/Tope" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "internal/mod/value"
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add"
                                , dummyWord "internal/mod/value"
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module and function aliases in type match" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "ext", "/mod" ) ]
                                , imports = Dict.empty
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "internal", "internal/mod" ) ]
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.InternalRef [ "ext" ] "Tipe" []) []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.InternalRef [ "internal" ] "Tope" []) []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop" ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/external/package/mod/Tipe") []
                                              , [ Builtin emptyRange Builtin.StackDrop ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "internal/mod/Tope") []
                                              , [ Builtin emptyRange Builtin.StackDrop ]
                                              )
                                            ]
                                            [ Builtin emptyRange Builtin.StackDrop ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Type definition aliases" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "ext", "/mod" )
                                        , ( "mod", "internal/mod" )
                                        ]
                                , imports = Dict.empty
                                , exposes = Set.empty
                                }
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Tepip"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "first", AST.InternalRef [ "ext" ] "Tipe" [] )
                                            , ( "second", AST.InternalRef [ "mod" ] "Tope" [] )
                                            ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Tepip"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "first", Type.Custom "/external/package/mod/Tipe" )
                                            , ( "second", Type.Custom "internal/mod/Tope" )
                                            ]
                                  }
                                ]
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module imports" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports =
                                    Dict.fromList
                                        [ ( "/mod", [ "add", "Tipe" ] )
                                        , ( "internal/mod", [] )
                                        ]
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "Tipe" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "Tope" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "value"
                                            , AST.Function emptyRange "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "/external/package/mod/Tipe" ]
                                            , output = [ Type.Custom "internal/mod/Tope" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "internal/mod/value"
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add"
                                , dummyWord "internal/mod/value"
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module and function imports" <|
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
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "Tipe" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "Tope" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.fromList [ ( "/mod", [ "add", "Tipe" ] ) ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "value"
                                            , AST.Function emptyRange "add"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "/external/package/mod/Tipe" ]
                                            , output = [ Type.Custom "internal/mod/Tope" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "internal/mod/value"
                                            , Function emptyRange "/external/package/mod/add"
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions =
                            Dict.fromList
                                [ dummyWord "/external/package/mod/add"
                                , dummyWord "internal/mod/value"
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Type definition imports" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports =
                                    Dict.fromList
                                        [ ( "/mod", [] )
                                        , ( "internal/mod", [ "Tope" ] )
                                        ]
                                , exposes = Set.empty
                                }
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Tepip"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "first", AST.LocalRef "Tipe" [] )
                                            , ( "second", AST.LocalRef "Tope" [] )
                                            ]
                                  }
                                ]
                        , functions = Dict.empty
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Tepip"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "first", Type.Custom "/external/package/mod/Tipe" )
                                            , ( "second", Type.Custom "internal/mod/Tope" )
                                            ]
                                  }
                                ]
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "Module and function imports in type match" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases = Dict.empty
                                , imports =
                                    Dict.fromList
                                        [ ( "/mod", [ "Tipe" ] ) ]
                                , exposes = Set.empty
                                }
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports =
                                        Dict.fromList
                                            [ ( "internal/mod", [] ) ]
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Tipe" []) []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.LocalRef "Tope" []) []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop" ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "external-call"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/external/package/mod/Tipe") []
                                              , [ Builtin emptyRange Builtin.StackDrop ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "internal/mod/Tope") []
                                              , [ Builtin emptyRange Builtin.StackDrop ]
                                              )
                                            ]
                                            [ Builtin emptyRange Builtin.StackDrop ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }

                    inProgressAst =
                        { types =
                            [ dummyType "/external/package/mod/Tipe"
                            , dummyType "internal/mod/Tope"
                            ]
                                |> List.concat
                                |> Dict.fromList
                        , functions = Dict.empty
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectExternalOutput
                    inProgressAst
                    unqualifiedAst
                    expectedAst
        , test "When module doesn't have a definition, all functions are exposed" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
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
                                            [ AST.Integer emptyRange 1
                                            , AST.Integer emptyRange 2
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "fn2"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 2
                                            , AST.Integer emptyRange 3
                                            , AST.Function emptyRange "+"
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
                                  , exposed = False
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
        , test "When module does have a definition, only functions defined to be exposed are" <|
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
                                            [ AST.Integer emptyRange 1
                                            , AST.Integer emptyRange 2
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "fn2"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 2
                                            , AST.Integer emptyRange 3
                                            , AST.Function emptyRange "+"
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
                                  , exposed = False
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
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "value"
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
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "add"
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
                                            [ AST.Function emptyRange "drop"
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
                                  , sourceLocation = emptyRange
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
                                            [ ( AST.TypeMatch emptyRange (AST.ExternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
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
                                            [ AST.Function emptyRange "drop"
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
                                  , sourceLocation = emptyRange
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
                                            [ ( AST.TypeMatch emptyRange (AST.InternalRef [ "mod" ] "Tipe" []) []
                                              , [ AST.Function emptyRange "drop"
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
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
        , test "There's an implicit import on standard_library/core in anonymous modules" <|
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
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "over"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "/stabel/standard_library/core/over" ]
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/core", "stabel/standard_library" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        , test "There's an implicit import on standard_library/core in named modules" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { imports = Dict.empty
                                , aliases = Dict.empty
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
                                            [ AST.Function emptyRange "over"
                                            ]
                                  }
                                ]
                        }

                    inProgressAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromList
                                [ dummyWord "/stabel/standard_library/core/over" ]
                        , referenceableFunctions = Set.empty
                        }

                    result =
                        run
                            { packageName = ""
                            , modulePath = ""
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/core", "stabel/standard_library" ) ]
                            , inProgressAST = inProgressAst
                            }
                in
                case result of
                    Ok _ ->
                        Expect.pass

                    Err errs ->
                        Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
        ]


dummyWord : String -> ( String, FunctionDefinition )
dummyWord name =
    dummyWordImpl name True


dummyWordUnexposed : String -> ( String, FunctionDefinition )
dummyWordUnexposed name =
    dummyWordImpl name False


dummyWordImpl : String -> Bool -> ( String, FunctionDefinition )
dummyWordImpl name isExposed =
    ( name
    , { name = name
      , exposed = isExposed
      , sourceLocation = Nothing
      , typeSignature = TypeSignature.NotProvided
      , implementation =
            SoloImpl []
      }
    )


dummyType : String -> List ( String, TypeDefinition )
dummyType name =
    dummyTypeImpl name True


dummyTypeUnexposed : String -> List ( String, TypeDefinition )
dummyTypeUnexposed name =
    dummyTypeImpl name False


dummyTypeImpl : String -> Bool -> List ( String, TypeDefinition )
dummyTypeImpl name isExposed =
    let
        genericName =
            name ++ "Generic"

        unionName =
            name ++ "Union"
    in
    List.map (\tipe -> ( tipe.name, tipe ))
        [ { name = name
          , exposed = isExposed
          , sourceLocation = emptyRange
          , generics = []
          , members = StructMembers []
          }
        , { name = genericName
          , exposed = isExposed
          , sourceLocation = emptyRange
          , generics = [ "a" ]
          , members = StructMembers []
          }
        , { name = unionName
          , exposed = isExposed
          , sourceLocation = emptyRange
          , generics = []
          , members =
                UnionMembers
                    [ Type.Custom name
                    , Type.CustomGeneric genericName [ Type.Generic "a" ]
                    ]
          }
        ]
