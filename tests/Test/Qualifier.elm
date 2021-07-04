module Test.Qualifier exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util as ParserUtil
import Test.Qualifier.Util as QualifierUtil


suite : Test
suite =
    describe "Qualifier"
        [ test "Simple program" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "inc"
                                            , AST.Function emptyRange "inc"
                                            , AST.Function emptyRange "dec"
                                            , AST.Integer emptyRange 2
                                            , AST.Function emptyRange "="
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Function emptyRange "inc"
                                            , Function emptyRange "inc"
                                            , Function emptyRange "dec"
                                            , Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectOutput unqualifiedAst expectedAst
        , test "Generic function types" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input =
                                                [ AST.NotStackRange <| AST.Generic "a"
                                                , AST.NotStackRange <| AST.Generic "b"
                                                ]
                                            , output =
                                                [ AST.NotStackRange <| AST.Generic "a"
                                                , AST.NotStackRange <| AST.Generic "b"
                                                , AST.NotStackRange <| AST.Generic "a"
                                                ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "swap"
                                            , AST.Function emptyRange "dup"
                                            , AST.Function emptyRange "rotate"
                                            ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Generic "a", Type.Generic "b" ]
                                            , output =
                                                [ Type.Generic "a"
                                                , Type.Generic "b"
                                                , Type.Generic "a"
                                                ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Builtin emptyRange Builtin.StackSwap
                                            , Builtin emptyRange Builtin.StackDuplicate
                                            , Builtin emptyRange Builtin.StackRightRotate
                                            ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectOutput unqualifiedAst expectedAst
        , test "Union types and multifunctions" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "True" []
                                            , AST.LocalRef "False" []
                                            ]
                                  }
                                , { name = "True"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                , { name = "False"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "to-int"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "False" []) [], [ AST.Integer emptyRange 0 ] )
                                            , ( AST.TypeMatch emptyRange (AST.LocalRef "True" []) [], [ AST.Integer emptyRange 1 ] )
                                            ]
                                            []
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        UnionMembers
                                            [ Type.Custom "True"
                                            , Type.Custom "False"
                                            ]
                                  }
                                , { name = "True"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                , { name = "False"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "to-int"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "False") [], [ Integer emptyRange 0 ] )
                                            , ( TypeMatch emptyRange (Type.Custom "True") [], [ Integer emptyRange 1 ] )
                                            ]
                                            []
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                QualifierUtil.expectOutput unqualifiedAst expectedAst
        , describe "Quotations"
            [ test "Basic case" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , typeSignature =
                                            AssociatedFunctionSignature.UserProvided
                                                { input =
                                                    [ AST.NotStackRange <| AST.LocalRef "Int" []
                                                    , AST.NotStackRange <|
                                                        AST.FunctionType
                                                            { input = [ AST.NotStackRange <| AST.LocalRef "Int" [] ]
                                                            , output = [ AST.NotStackRange <| AST.LocalRef "Int" [] ]
                                                            }
                                                    ]
                                                , output = [ AST.NotStackRange <| AST.LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Function emptyRange "!"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.InlineFunction emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Function emptyRange "+"
                                                    ]
                                                , AST.Function emptyRange "apply-to-num"
                                                , AST.InlineFunction emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Function emptyRange "-"
                                                    ]
                                                , AST.Function emptyRange "apply-to-num"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
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
                                            SoloImpl
                                                [ Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , FunctionRef emptyRange "inlinefn:main/2"
                                                , Function emptyRange "apply-to-num"
                                                , FunctionRef emptyRange "inlinefn:main/1"
                                                , Function emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "inlinefn:main/2"
                                      , exposed = False
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "inlinefn:main/1"
                                      , exposed = False
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Minus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "inlinefn:main/2"
                                    , "inlinefn:main/1"
                                    ]
                            }
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            , test "Quoting a word which hasn't been qualified yet is fine" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "a"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.InlineFunction emptyRange
                                                    [ AST.Function emptyRange "inc"
                                                    ]
                                                , AST.Function emptyRange "!"
                                                ]
                                      }
                                    , { name = "inc"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "+"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "a"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , FunctionRef emptyRange "inc"
                                                , Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "inc"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "inc" ]
                            }
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            , test "Quotes within quotes is fine" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.InlineFunction emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.InlineFunction emptyRange
                                                        [ AST.Integer emptyRange 1
                                                        , AST.Function emptyRange "+"
                                                        ]
                                                    , AST.Function emptyRange "!"
                                                    , AST.Function emptyRange "+"
                                                    ]
                                                , AST.Function emptyRange "!"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , FunctionRef emptyRange "inlinefn:main/1"
                                                , Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "inlinefn:main/1"
                                      , exposed = False
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , FunctionRef emptyRange "inlinefn:main/1/1"
                                                , Builtin emptyRange Builtin.Apply
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "inlinefn:main/1/1"
                                      , exposed = False
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            , referenceableFunctions =
                                Set.fromList
                                    [ "inlinefn:main/1"
                                    , "inlinefn:main/1/1"
                                    ]
                            }
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            ]
        , describe "Pattern matching"
            [ test "Basic example" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Bool"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            AST.UnionMembers
                                                [ AST.LocalRef "True" []
                                                , AST.LocalRef "False" []
                                                ]
                                      }
                                    , { name = "True"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    , { name = "False"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    , { name = "Box"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            AST.StructMembers
                                                [ ( "value", AST.LocalRef "Int" [] ) ]
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.LocalRef "Box" []) [ ( "value", AST.LiteralInt 0 ) ]
                                                  , [ AST.Function emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Function emptyRange "False" ]
                                      }
                                    ]
                            }
                                |> ParserUtil.addFunctionsForStructs

                        expectedAst =
                            { types =
                                Dict.fromListBy .name
                                    [ { name = "Bool"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            UnionMembers
                                                [ Type.Custom "True"
                                                , Type.Custom "False"
                                                ]
                                      }
                                    , { name = "True"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = StructMembers []
                                      }
                                    , { name = "False"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = StructMembers []
                                      }
                                    , { name = "Box"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            StructMembers
                                                [ ( "value", Type.Int ) ]
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.Custom "Box") [ ( "value", LiteralInt 0 ) ], [ Function emptyRange "True" ] )
                                                ]
                                                [ Function emptyRange "False" ]
                                      }
                                    ]
                            , referenceableFunctions = Set.empty
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            , test "Generic cases are allowed" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Maybe"
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            AST.UnionMembers
                                                [ AST.Generic "a"
                                                , AST.LocalRef "Nothing" []
                                                ]
                                      }
                                    , { name = "Nothing"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "with-default"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.Generic "a") []
                                                  , [ AST.Function emptyRange "drop" ]
                                                  )
                                                ]
                                                [ AST.Function emptyRange "swap"
                                                , AST.Function emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }
                                |> ParserUtil.addFunctionsForStructs

                        expectedAst =
                            { types =
                                Dict.fromListBy .name
                                    [ { name = "Maybe"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = [ "a" ]
                                      , members =
                                            UnionMembers
                                                [ Type.Generic "a"
                                                , Type.Custom "Nothing"
                                                ]
                                      }
                                    , { name = "Nothing"
                                      , exposed = True
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members = StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "with-default"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.Generic "a") []
                                                  , [ Builtin emptyRange Builtin.StackDrop ]
                                                  )
                                                ]
                                                [ Builtin emptyRange Builtin.StackSwap
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                      }
                                    ]
                            , referenceableFunctions = Set.empty
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            ]
        , test "Resolves unions" <|
            \_ ->
                let
                    boolUnion =
                        Type.Union (Just "Bool")
                            [ Type.Custom "True"
                            , Type.Custom "False"
                            ]

                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "True" []
                                            , AST.LocalRef "False" []
                                            ]
                                  }
                                , { name = "True"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                , { name = "False"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                , { name = "Box"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "value", AST.LocalRef "Bool" [] ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "true?"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "Box" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "Bool" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange
                                                    (AST.LocalRef "Box" [])
                                                    [ ( "value", AST.LiteralType (AST.LocalRef "True" []) ) ]
                                              , [ AST.Function emptyRange "True" ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "False" ]
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        UnionMembers
                                            [ Type.Custom "True"
                                            , Type.Custom "False"
                                            ]
                                  }
                                , { name = "True"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                , { name = "False"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = StructMembers []
                                  }
                                , { name = "Box"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "value", boolUnion ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "true?"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "Box" ]
                                            , output = [ boolUnion ]
                                            }
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange
                                                    (Type.Custom "Box")
                                                    [ ( "value", LiteralType (Type.Custom "True") ) ]
                                              , [ Function emptyRange "True" ]
                                              )
                                            ]
                                            [ Function emptyRange "False" ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                            |> QualifierUtil.addFunctionsForStructs
                in
                QualifierUtil.expectOutput unqualifiedAst expectedAst
        , test "Name mangling" <|
            \_ ->
                let
                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    qualifiedUsMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            qualifiedUsMoneyUnion

                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "USMoney"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "Dollar" []
                                            , AST.LocalRef "Cent" []
                                            ]
                                  }
                                , { name = "Dollar"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                  }
                                , { name = "Cent"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "into-cents"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Dollar" []) []
                                              , [ AST.Function emptyRange "dollar-value>"
                                                , AST.Integer emptyRange 100
                                                , AST.Function emptyRange "*"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.LocalRef "Cent" []) []
                                              , [ AST.Function emptyRange "cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "add-money"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "USMoney" [], AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "into-cents"
                                            , AST.Function emptyRange "swap"
                                            , AST.Function emptyRange "into-cents"
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "quote-excuse"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "Dollar" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "Dollar" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function emptyRange "dollar-value>"
                                            , AST.InlineFunction emptyRange
                                                [ AST.Integer emptyRange 2
                                                , AST.Function emptyRange "*"
                                                ]
                                            , AST.Function emptyRange "!"
                                            , AST.Function emptyRange ">Dollar"
                                            ]
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/USMoney"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = UnionMembers qualifiedUsMoneyUnion
                                  }
                                , { name = "/stabel/test/some/module/Dollar"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "dollar-value", Type.Int ) ]
                                  }
                                , { name = "/stabel/test/some/module/Cent"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "cent-value", Type.Int ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/into-cents"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ qualifiedUsMoneyUnionType ]
                                            , output = [ qualifiedUsMoneyUnionType ]
                                            }
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Dollar") []
                                              , [ Function emptyRange "/stabel/test/some/module/dollar-value>"
                                                , Integer emptyRange 100
                                                , Builtin emptyRange Builtin.Multiply
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Cent") []
                                              , [ Function emptyRange "/stabel/test/some/module/cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "/stabel/test/some/module/add-money"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ qualifiedUsMoneyUnionType, qualifiedUsMoneyUnionType ]
                                            , output = [ qualifiedUsMoneyUnionType ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Function emptyRange "/stabel/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.StackSwap
                                            , Function emptyRange "/stabel/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "/stabel/test/some/module/quote-excuse"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.UserProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ Function emptyRange "/stabel/test/some/module/dollar-value>"
                                            , FunctionRef emptyRange "inlinefn:/stabel/test/some/module/quote-excuse/1"
                                            , Builtin emptyRange Builtin.Apply
                                            , Function emptyRange "/stabel/test/some/module/>Dollar"
                                            ]
                                  }
                                , { name = "inlinefn:/stabel/test/some/module/quote-excuse/1"
                                  , exposed = False
                                  , sourceLocation = Nothing
                                  , typeSignature = TypeSignature.NotProvided
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Multiply
                                            ]
                                  }
                                , { name = "/stabel/test/some/module/>Dollar"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            }
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Dollar" ]
                                  }
                                , { name = "/stabel/test/some/module/>Cent"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            }
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Cent" ]
                                  }
                                , { name = "/stabel/test/some/module/>dollar-value"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Dollar", Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>cent-value"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Cent", Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/dollar-value>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/cent-value>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                ]
                        , referenceableFunctions =
                            Set.fromList
                                [ "inlinefn:/stabel/test/some/module/quote-excuse/1" ]
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , test "member types are qualified" <|
            \_ ->
                let
                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    qualifiedUsMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            qualifiedUsMoneyUnion

                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "USMoney"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "Dollar" []
                                            , AST.LocalRef "Cent" []
                                            ]
                                  }
                                , { name = "Wallet"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "user-id", AST.LocalRef "Int" [] )
                                            , ( "value", AST.LocalRef "USMoney" [] )
                                            ]
                                  }
                                , { name = "Dollar"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                  }
                                , { name = "Cent"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/USMoney"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members = UnionMembers qualifiedUsMoneyUnion
                                  }
                                , { name = "/stabel/test/some/module/Dollar"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "dollar-value", Type.Int ) ]
                                  }
                                , { name = "/stabel/test/some/module/Cent"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "cent-value", Type.Int ) ]
                                  }
                                , { name = "/stabel/test/some/module/Wallet"
                                  , exposed = True
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        StructMembers
                                            [ ( "user-id", Type.Int )
                                            , ( "value", qualifiedUsMoneyUnionType )
                                            ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/>Dollar"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            }
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Dollar" ]
                                  }
                                , { name = "/stabel/test/some/module/>Cent"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            }
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Cent" ]
                                  }
                                , { name = "/stabel/test/some/module/>Wallet"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Int, qualifiedUsMoneyUnionType ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                            }
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Wallet" ]
                                  }
                                , { name = "/stabel/test/some/module/>dollar-value"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Dollar", Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>cent-value"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Cent", Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>user-id"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Wallet", Type.Int ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/stabel/test/some/module/>value"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Wallet", qualifiedUsMoneyUnionType ]
                                            , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Wallet" "value" ]
                                  }
                                , { name = "/stabel/test/some/module/dollar-value>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/cent-value>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/user-id>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                            , output = [ Type.Int ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/stabel/test/some/module/value>"
                                  , exposed = True
                                  , sourceLocation = Nothing
                                  , typeSignature =
                                        TypeSignature.CompilerProvided
                                            { input = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                            , output = [ qualifiedUsMoneyUnionType ]
                                            }
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Wallet" "value" ]
                                  }
                                ]
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , test "Retrieve dependant modules" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "html", "/external/html" ) ]
                                , imports =
                                    Dict.fromList
                                        [ ( "/external/module", [] ) ]
                                , exposes = Set.fromList []
                                }
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Tipe"
                                  , sourceLocation = emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "value", AST.ExternalRef [ "external", "double" ] "Tipe" [] ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "call-external"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "internal", "types" ] "In" [] ]
                                            , output = [ AST.NotStackRange <| AST.ExternalRef [ "external", "types" ] "Out" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) [ ( "value", AST.LiteralInt 1 ) ]
                                              , [ AST.PackageFunction emptyRange [ "package", "module" ] "when-one"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.InternalRef [ "internal", "match" ] "Some" []) []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            ]
                                            [ AST.PackageFunction emptyRange [ "package", "module" ] "when-other-one" ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "ali", "internal/alias" ) ]
                                  , imports = Dict.fromList [ ( "/list/of/names", [ "one" ] ) ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.PackageFunction emptyRange [ "html" ] "div"
                                            , AST.Function emptyRange "call-external"
                                            , AST.ExternalFunction emptyRange [ "some", "ext" ] "word"
                                            , AST.PackageFunction emptyRange [ "ali" ] "word1"
                                            ]
                                  }
                                ]
                        }

                    expectedRequiredModules =
                        Set.fromList
                            [ "/robheghan/dummy/list/of/names"
                            , "/robheghan/dummy/some/ext"
                            , "/robheghan/html/external/html"
                            , "/robheghan/html/external/module"
                            , "/robheghan/html/external/types"
                            , "/robheghan/html/external/double"
                            , "/package/test/internal/alias"
                            , "/package/test/internal/types"
                            , "/package/test/internal/match"
                            , "/package/test/package/module"
                            , "/stabel/standard_library/core"
                            ]

                    actualRequiredModules =
                        requiredModules
                            { packageName = "package/test"
                            , ast = unqualifiedAst
                            , externalModules =
                                Dict.fromList
                                    [ ( "/list/of/names", "robheghan/dummy" )
                                    , ( "/some/ext", "robheghan/dummy" )
                                    , ( "/external/html", "robheghan/html" )
                                    , ( "/external/module", "robheghan/html" )
                                    , ( "/external/types", "robheghan/html" )
                                    , ( "/external/double", "robheghan/html" )
                                    , ( "/core", "stabel/standard_library" )
                                    ]
                            }
                in
                Expect.equal expectedRequiredModules actualRequiredModules
        , test "Reliance on standard_library/core only when standard_library is specified as externalModule" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "ali", "internal/alias" ) ]
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.PackageFunction emptyRange [ "ali" ] "word1"
                                            ]
                                  }
                                ]
                        }

                    expectedRequiredModules =
                        Set.fromList
                            [ "/package/test/internal/alias"
                            ]

                    actualRequiredModules =
                        requiredModules
                            { packageName = "package/test"
                            , ast = unqualifiedAst
                            , externalModules = Dict.empty
                            }
                in
                Expect.equal expectedRequiredModules actualRequiredModules
        ]
