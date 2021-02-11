module Test.Qualifier exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Builtin as Builtin
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type
import Play.Parser as AST
import Play.Qualifier exposing (..)
import Set
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
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "inc"
                                            , AST.Word emptyRange "inc"
                                            , AST.Word emptyRange "dec"
                                            , AST.Integer emptyRange 2
                                            , AST.Word emptyRange "="
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
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
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
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "a", Type.Generic "b", Type.Generic "a" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Word emptyRange "swap"
                                            , AST.Word emptyRange "dup"
                                            , AST.Word emptyRange "rotate"
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
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "Bool"
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , AST.CustomTypeDef emptyRange "True" [] []
                                , AST.CustomTypeDef emptyRange "False" [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "to-int"
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (Type.Custom "False") [], [ AST.Integer emptyRange 0 ] )
                                            , ( AST.TypeMatch emptyRange (Type.Custom "True") [], [ AST.Integer emptyRange 1 ] )
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
                                            AST.SoloImpl
                                                [ AST.Word emptyRange "!"
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Quotation emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Word emptyRange "+"
                                                    ]
                                                , AST.Word emptyRange "apply-to-num"
                                                , AST.Quotation emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Word emptyRange "-"
                                                    ]
                                                , AST.Word emptyRange "apply-to-num"
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
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
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
            , test "Do not create new function if quoting exactly one word" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Quotation emptyRange
                                                    [ AST.Word emptyRange "inc"
                                                    ]
                                                , AST.Word emptyRange "!"
                                                ]
                                      }
                                    , { name = "inc"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , WordRef emptyRange "inc"
                                                , Builtin emptyRange Builtin.Apply
                                                ]
                                      }
                                    , { name = "inc"
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
            , test "Quotes within quotes is fine" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Quotation emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Quotation emptyRange
                                                        [ AST.Integer emptyRange 1
                                                        , AST.Word emptyRange "+"
                                                        ]
                                                    , AST.Word emptyRange "!"
                                                    , AST.Word emptyRange "+"
                                                    ]
                                                , AST.Word emptyRange "!"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
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
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef emptyRange
                                        "Bool"
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , AST.CustomTypeDef emptyRange "True" [] []
                                    , AST.CustomTypeDef emptyRange "False" [] []
                                    , AST.CustomTypeDef emptyRange
                                        "Box"
                                        []
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (Type.Custom "Box") [ ( "value", AST.LiteralInt 0 ) ]
                                                  , [ AST.Word emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "False" ]
                                      }
                                    ]
                            }
                                |> ParserUtil.addFunctionsForStructs

                        expectedAst =
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
                                    , CustomTypeDef "Box"
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
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef emptyRange
                                        "Maybe"
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nothing"
                                        ]
                                    , AST.CustomTypeDef emptyRange "Nothing" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "with-default"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (Type.Generic "a") []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "swap"
                                                , AST.Word emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }
                                |> ParserUtil.addFunctionsForStructs

                        expectedAst =
                            { types =
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Maybe"
                                        emptyRange
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nothing"
                                        ]
                                    , CustomTypeDef "Nothing" emptyRange [] []
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
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "Bool"
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , AST.CustomTypeDef emptyRange "True" [] []
                                , AST.CustomTypeDef emptyRange "False" [] []
                                , AST.CustomTypeDef emptyRange
                                    "Box"
                                    []
                                    [ ( "value", Type.Custom "Bool" ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "true?"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Box" ] [ Type.Custom "Bool" ]
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange
                                                    (Type.Custom "Box")
                                                    [ ( "value", AST.LiteralType (Type.Custom "True") ) ]
                                              , [ AST.Word emptyRange "True" ]
                                              )
                                            ]
                                            [ AST.Word emptyRange "False" ]
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
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
                                , CustomTypeDef "Box"
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
                        [ Type.Custom "/play/test/some/module/Dollar"
                        , Type.Custom "/play/test/some/module/Cent"
                        ]

                    unqualifiedAst =
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "USMoney"
                                    []
                                    [ Type.Custom "Dollar"
                                    , Type.Custom "Cent"
                                    ]
                                , AST.CustomTypeDef emptyRange
                                    "Dollar"
                                    []
                                    [ ( "dollar-value", Type.Int ) ]
                                , AST.CustomTypeDef emptyRange
                                    "Cent"
                                    []
                                    [ ( "cent-value", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "into-cents"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Custom "USMoney" ]
                                                [ Type.Custom "USMoney" ]
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (Type.Custom "Dollar") []
                                              , [ AST.Word emptyRange "dollar-value>"
                                                , AST.Integer emptyRange 100
                                                , AST.Word emptyRange "*"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (Type.Custom "Cent") []
                                              , [ AST.Word emptyRange "cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "add-money"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Custom "USMoney", Type.Custom "USMoney" ]
                                                [ Type.Custom "USMoney" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Word emptyRange "into-cents"
                                            , AST.Word emptyRange "swap"
                                            , AST.Word emptyRange "into-cents"
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "quote-excuse"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Dollar" ] [ Type.Custom "Dollar" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Word emptyRange "dollar-value>"
                                            , AST.Quotation emptyRange
                                                [ AST.Integer emptyRange 2
                                                , AST.Word emptyRange "*"
                                                ]
                                            , AST.Word emptyRange "!"
                                            , AST.Word emptyRange ">Dollar"
                                            ]
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

                    expectedAst =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef
                                    "/play/test/some/module/USMoney"
                                    emptyRange
                                    []
                                    qualifiedUsMoneyUnion
                                , CustomTypeDef "/play/test/some/module/Dollar"
                                    emptyRange
                                    []
                                    [ ( "dollar-value", Type.Int ) ]
                                , CustomTypeDef "/play/test/some/module/Cent"
                                    emptyRange
                                    []
                                    [ ( "cent-value", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "/play/test/some/module/into-cents"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (Type.Custom "/play/test/some/module/Dollar") []
                                              , [ Word emptyRange "/play/test/some/module/dollar-value>"
                                                , Integer emptyRange 100
                                                , Builtin emptyRange Builtin.Multiply
                                                ]
                                              )
                                            , ( TypeMatch emptyRange (Type.Custom "/play/test/some/module/Cent") []
                                              , [ Word emptyRange "/play/test/some/module/cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "/play/test/some/module/add-money"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Union qualifiedUsMoneyUnion, Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        SoloImpl
                                            [ Word emptyRange "/play/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.StackSwap
                                            , Word emptyRange "/play/test/some/module/into-cents"
                                            , Builtin emptyRange Builtin.Plus
                                            ]
                                  }
                                , { name = "/play/test/some/module/quote-excuse"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Custom "/play/test/some/module/Dollar" ]
                                                [ Type.Custom "/play/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl
                                            [ Word emptyRange "/play/test/some/module/dollar-value>"
                                            , WordRef emptyRange "quote:/play/test/some/module/quote-excuse/1"
                                            , Builtin emptyRange Builtin.Apply
                                            , Word emptyRange "/play/test/some/module/>Dollar"
                                            ]
                                  }
                                , { name = "quote:/play/test/some/module/quote-excuse/1"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.isQuoted
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            , Builtin emptyRange Builtin.Multiply
                                            ]
                                  }
                                , { name = "/play/test/some/module/>Dollar"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/play/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/play/test/some/module/Dollar" ]
                                  }
                                , { name = "/play/test/some/module/>Cent"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "/play/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/play/test/some/module/Cent" ]
                                  }
                                , { name = "/play/test/some/module/>dollar-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/play/test/some/module/Dollar", Type.Int ]
                                                [ Type.Custom "/play/test/some/module/Dollar" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/play/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/play/test/some/module/>cent-value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/play/test/some/module/Cent", Type.Int ]
                                                [ Type.Custom "/play/test/some/module/Cent" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/play/test/some/module/Cent" "cent-value" ]
                                  }
                                , { name = "/play/test/some/module/dollar-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/play/test/some/module/Dollar" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/play/test/some/module/Dollar" "dollar-value" ]
                                  }
                                , { name = "/play/test/some/module/cent-value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/play/test/some/module/Cent" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/play/test/some/module/Cent" "cent-value" ]
                                  }
                                ]
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , describe "Module loading"
            [ test "Detects package reference in simple word" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "call-external"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.PackageWord emptyRange [ "package", "module" ] "sample"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { additionalModulesRequired =
                                Set.fromList
                                    [ "/play/test/package/module" ]
                            , checkForExistingTypes = Set.empty
                            , checkForExistingWords =
                                Set.fromList
                                    [ "/play/test/package/module/sample" ]
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "/play/test/package/tests/call-external"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "/play/test/package/module/sample"
                                                ]
                                      }
                                    ]
                            }

                        result =
                            run
                                { packageName = "play/test"
                                , modulePath = "package/tests"
                                , ast = unqualifiedAst
                                , externalModules = []
                                }
                    in
                    case result of
                        Err err ->
                            Expect.fail <| "Did not expect qualification to fail with error: " ++ Debug.toString err

                        Ok actualAst ->
                            Expect.equal expectedAst actualAst
            , test "Detects package reference in multiword" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "call-external"
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange Type.Int [ ( "value", AST.LiteralInt 1 ) ]
                                                  , [ AST.PackageWord emptyRange [ "package", "module" ] "when-one"
                                                    ]
                                                  )
                                                ]
                                                [ AST.PackageWord emptyRange [ "package", "module" ] "when-other-one" ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { additionalModulesRequired =
                                Set.fromList
                                    [ "/play/test/package/module" ]
                            , checkForExistingTypes = Set.empty
                            , checkForExistingWords =
                                Set.fromList
                                    [ "/play/test/package/module/when-one"
                                    , "/play/test/package/module/when-other-one"
                                    ]
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "/play/test/package/tests/call-external"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int [ ( "value", LiteralInt 1 ) ]
                                                  , [ Word emptyRange "/play/test/package/module/when-one"
                                                    ]
                                                  )
                                                ]
                                                [ Word emptyRange "/play/test/package/module/when-other-one" ]
                                      }
                                    ]
                            }

                        result =
                            run
                                { packageName = "play/test"
                                , modulePath = "package/tests"
                                , ast = unqualifiedAst
                                , externalModules = []
                                }
                    in
                    case result of
                        Err err ->
                            Expect.fail <| "Did not expect qualification to fail with error: " ++ Debug.toString err

                        Ok actualAst ->
                            Expect.equal expectedAst actualAst
            ]
        ]
