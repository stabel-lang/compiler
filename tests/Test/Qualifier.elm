module Test.Qualifier exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.Metadata as Metadata
import Stabel.Data.Type as Type
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.SourceLocation exposing (emptyRange)
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
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Builtin emptyRange Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Word emptyRange "inc"
                                            , Word emptyRange "inc"
                                            , Word emptyRange "dec"
                                            , Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Equal
                                            ]
                                  }
                                ]
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
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "a"
                                                , Type.Generic "b"
                                                , Type.Generic "a"
                                                ]
                                  , implementation =
                                        SoloImpl
                                            [ Builtin emptyRange Builtin.StackSwap
                                            , Builtin emptyRange Builtin.StackDuplicate
                                            , Builtin emptyRange Builtin.StackRightRotate
                                            ]
                                  }
                                ]
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
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    True
                                    emptyRange
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" True emptyRange [] []
                                , CustomTypeDef "False" True emptyRange [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "to-int"
                                  , metadata = Metadata.default
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "False") [], [ Integer emptyRange 0 ] )
                                            , ( TypeMatch emptyRange (Type.Custom "True") [], [ Integer emptyRange 1 ] )
                                            ]
                                            []
                                  }
                                ]
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
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Int
                                                    , Type.Quotation { input = [ Type.Int ], output = [ Type.Int ] }
                                                    ]
                                                    [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , WordRef emptyRange "quote:main/2"
                                                , Word emptyRange "apply-to-num"
                                                , WordRef emptyRange "quote:main/1"
                                                , Word emptyRange "apply-to-num"
                                                ]
                                      }
                                    , { name = "quote:main/2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "quote:main/1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Minus
                                                ]
                                      }
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
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "a"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , WordRef emptyRange "quote:a/1"
                                                , Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "inc"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "quote:a/1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Word emptyRange "inc"
                                                ]
                                      }
                                    ]
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
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , WordRef emptyRange "quote:main/1"
                                                , Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "quote:main/1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , WordRef emptyRange "quote:main/1/1"
                                                , Builtin emptyRange Builtin.Apply
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "quote:main/1/1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
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
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Bool"
                                        True
                                        emptyRange
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , CustomTypeDef "True" True emptyRange [] []
                                    , CustomTypeDef "False" True emptyRange [] []
                                    , CustomTypeDef "Box"
                                        True
                                        emptyRange
                                        []
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.Custom "Box") [ ( "value", LiteralInt 0 ) ], [ Word emptyRange "True" ] )
                                                ]
                                                [ Word emptyRange "False" ]
                                      }
                                    ]
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
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Maybe"
                                        True
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nothing"
                                        ]
                                    , CustomTypeDef "Nothing" True emptyRange [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "with-default"
                                      , metadata = Metadata.default
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
                            }
                                |> QualifierUtil.addFunctionsForStructs
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            ]
        , test "Resolves unions" <|
            \_ ->
                let
                    boolUnion =
                        Type.Union
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
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    True
                                    emptyRange
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" True emptyRange [] []
                                , CustomTypeDef "False" True emptyRange [] []
                                , CustomTypeDef "Box"
                                    True
                                    emptyRange
                                    []
                                    [ ( "value", boolUnion ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "true?"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Box" ] [ boolUnion ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange
                                                    (Type.Custom "Box")
                                                    [ ( "value", LiteralType (Type.Custom "True") ) ]
                                              , [ Word emptyRange "True" ]
                                              )
                                            ]
                                            [ Word emptyRange "False" ]
                                  }
                                ]
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
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef
                                    "/stabel/test/some/module/USMoney"
                                    True
                                    emptyRange
                                    []
                                    qualifiedUsMoneyUnion
                                , CustomTypeDef "/stabel/test/some/module/Dollar"
                                    True
                                    emptyRange
                                    []
                                    [ ( "dollar-value", Type.Int ) ]
                                , CustomTypeDef "/stabel/test/some/module/Cent"
                                    True
                                    emptyRange
                                    []
                                    [ ( "cent-value", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/into-cents"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Dollar") []
                                              , [ Word emptyRange "/stabel/test/some/module/dollar-value>"
                                                , Integer emptyRange 100
                                                , Builtin emptyRange Builtin.Multiply
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Cent") []
                                              , [ Word emptyRange "/stabel/test/some/module/cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "/stabel/test/some/module/add-money"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Union qualifiedUsMoneyUnion, Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        SoloImpl
                                            [ Word emptyRange "/stabel/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.StackSwap
                                            , Word emptyRange "/stabel/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "/stabel/test/some/module/quote-excuse"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                                [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl
                                            [ Word emptyRange "/stabel/test/some/module/dollar-value>"
                                            , WordRef emptyRange "quote:/stabel/test/some/module/quote-excuse/1"
                                            , Builtin emptyRange Builtin.Apply
                                            , Word emptyRange "/stabel/test/some/module/>Dollar"
                                            ]
                                  }
                                , { name = "quote:/stabel/test/some/module/quote-excuse/1"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isQuoted
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Multiply
                                            ]
                                  }
                                , { name = "/stabel/test/some/module/>Dollar"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Dollar" ]
                                  }
                                , { name = "/stabel/test/some/module/>Cent"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/stabel/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Cent" ]
                                  }
                                , { name = "/stabel/test/some/module/>dollar-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Dollar", Type.Int ]
                                                [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>cent-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Cent", Type.Int ]
                                                [ Type.Custom "/stabel/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/dollar-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/stabel/test/some/module/Dollar" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/cent-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/stabel/test/some/module/Cent" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                ]
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
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef
                                    "/stabel/test/some/module/USMoney"
                                    True
                                    emptyRange
                                    []
                                    qualifiedUsMoneyUnion
                                , CustomTypeDef "/stabel/test/some/module/Dollar"
                                    True
                                    emptyRange
                                    []
                                    [ ( "dollar-value", Type.Int ) ]
                                , CustomTypeDef "/stabel/test/some/module/Cent"
                                    True
                                    emptyRange
                                    []
                                    [ ( "cent-value", Type.Int ) ]
                                , CustomTypeDef "/stabel/test/some/module/Wallet"
                                    True
                                    emptyRange
                                    []
                                    [ ( "user-id", Type.Int )
                                    , ( "value", Type.Union qualifiedUsMoneyUnion )
                                    ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "/stabel/test/some/module/>Dollar"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Dollar" ]
                                  }
                                , { name = "/stabel/test/some/module/>Cent"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/stabel/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Cent" ]
                                  }
                                , { name = "/stabel/test/some/module/>Wallet"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int, Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/stabel/test/some/module/Wallet" ]
                                  }
                                , { name = "/stabel/test/some/module/>dollar-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Dollar", Type.Int ]
                                                [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>cent-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Cent", Type.Int ]
                                                [ Type.Custom "/stabel/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/>user-id"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Wallet", Type.Int ]
                                                [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/stabel/test/some/module/>value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Wallet", Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/stabel/test/some/module/Wallet" "value" ]
                                  }
                                , { name = "/stabel/test/some/module/dollar-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/stabel/test/some/module/Dollar" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/stabel/test/some/module/cent-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/stabel/test/some/module/Cent" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/stabel/test/some/module/user-id>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/stabel/test/some/module/Wallet" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/stabel/test/some/module/value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/stabel/test/some/module/Wallet" "value" ]
                                  }
                                ]
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
