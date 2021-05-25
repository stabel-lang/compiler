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
import Play.Qualifier.Problem as Problem
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = AST.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , typeSignature = AST.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = AST.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.Generic "a", AST.Generic "b" ]
                                            , output = [ AST.Generic "a", AST.Generic "b", AST.Generic "a" ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "Bool"
                                    []
                                    [ AST.LocalRef "True" []
                                    , AST.LocalRef "False" []
                                    ]
                                , AST.CustomTypeDef emptyRange "True" [] []
                                , AST.CustomTypeDef emptyRange "False" [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "to-int"
                                  , typeSignature = AST.NotProvided
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input =
                                                    [ AST.LocalRef "Int" []
                                                    , AST.QuotationType
                                                        { input = [ AST.LocalRef "Int" [] ]
                                                        , output = [ AST.LocalRef "Int" [] ]
                                                        }
                                                    ]
                                                , output = [ AST.LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Word emptyRange "!"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "a"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
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
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef emptyRange
                                        "Bool"
                                        []
                                        [ AST.LocalRef "True" []
                                        , AST.LocalRef "False" []
                                        ]
                                    , AST.CustomTypeDef emptyRange "True" [] []
                                    , AST.CustomTypeDef emptyRange "False" [] []
                                    , AST.CustomTypeDef emptyRange
                                        "Box"
                                        []
                                        [ ( "value", AST.LocalRef "Int" [] ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.LocalRef "Box" []) [ ( "value", AST.LiteralInt 0 ) ]
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef emptyRange
                                        "Maybe"
                                        [ "a" ]
                                        [ AST.Generic "a"
                                        , AST.LocalRef "Nothing" []
                                        ]
                                    , AST.CustomTypeDef emptyRange "Nothing" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "with-default"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.Generic "a") []
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "Bool"
                                    []
                                    [ AST.LocalRef "True" []
                                    , AST.LocalRef "False" []
                                    ]
                                , AST.CustomTypeDef emptyRange "True" [] []
                                , AST.CustomTypeDef emptyRange "False" [] []
                                , AST.CustomTypeDef emptyRange
                                    "Box"
                                    []
                                    [ ( "value", AST.LocalRef "Bool" [] ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "true?"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "Box" [] ]
                                            , output = [ AST.LocalRef "Bool" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange
                                                    (AST.LocalRef "Box" [])
                                                    [ ( "value", AST.LiteralType (AST.LocalRef "True" []) ) ]
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "USMoney"
                                    []
                                    [ AST.LocalRef "Dollar" []
                                    , AST.LocalRef "Cent" []
                                    ]
                                , AST.CustomTypeDef emptyRange
                                    "Dollar"
                                    []
                                    [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                , AST.CustomTypeDef emptyRange
                                    "Cent"
                                    []
                                    [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "into-cents"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Dollar" []) []
                                              , [ AST.Word emptyRange "dollar-value>"
                                                , AST.Integer emptyRange 100
                                                , AST.Word emptyRange "*"
                                                ]
                                              )
                                            , ( AST.TypeMatch emptyRange (AST.LocalRef "Cent" []) []
                                              , [ AST.Word emptyRange "cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "add-money"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "USMoney" [], AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Word emptyRange "into-cents"
                                            , AST.Word emptyRange "swap"
                                            , AST.Word emptyRange "into-cents"
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "quote-excuse"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "Dollar" [] ]
                                            , output = [ AST.LocalRef "Dollar" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
        , test "member types are qualified" <|
            \_ ->
                let
                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/play/test/some/module/Dollar"
                        , Type.Custom "/play/test/some/module/Cent"
                        ]

                    unqualifiedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ AST.UnionTypeDef emptyRange
                                    "USMoney"
                                    []
                                    [ AST.LocalRef "Dollar" []
                                    , AST.LocalRef "Cent" []
                                    ]
                                , AST.CustomTypeDef emptyRange
                                    "Wallet"
                                    []
                                    [ ( "user-id", AST.LocalRef "Int" [] )
                                    , ( "value", AST.LocalRef "USMoney" [] )
                                    ]
                                , AST.CustomTypeDef emptyRange
                                    "Dollar"
                                    []
                                    [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                , AST.CustomTypeDef emptyRange
                                    "Cent"
                                    []
                                    [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                ]
                        , words = Dict.empty
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
                                , CustomTypeDef "/play/test/some/module/Wallet"
                                    emptyRange
                                    []
                                    [ ( "user-id", Type.Int )
                                    , ( "value", Type.Union qualifiedUsMoneyUnion )
                                    ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "/play/test/some/module/>Dollar"
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
                                , { name = "/play/test/some/module/>Wallet"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int, Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Custom "/play/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl [ ConstructType "/play/test/some/module/Wallet" ]
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
                                , { name = "/play/test/some/module/>user-id"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/play/test/some/module/Wallet", Type.Int ]
                                                [ Type.Custom "/play/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/play/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/play/test/some/module/>value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/play/test/some/module/Wallet", Type.Union qualifiedUsMoneyUnion ]
                                                [ Type.Custom "/play/test/some/module/Wallet" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "/play/test/some/module/Wallet" "value" ]
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
                                , { name = "/play/test/some/module/user-id>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "/play/test/some/module/Wallet" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/play/test/some/module/Wallet" "user-id" ]
                                  }
                                , { name = "/play/test/some/module/value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "/play/test/some/module/Wallet" ]
                                                [ Type.Union qualifiedUsMoneyUnion ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "/play/test/some/module/Wallet" "value" ]
                                  }
                                ]
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , test "Retrieve dependant modules" <|
            \_ ->
                let
                    unqualifiedAst =
                        { moduleDefinition =
                            AST.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "html", "/external/html" ) ]
                                , imports =
                                    Dict.fromList
                                        [ ( "/external/module", [] ) ]
                                , exposes = Set.fromList []
                                }
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "call-external"
                                  , typeSignature = AST.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) [ ( "value", AST.LiteralInt 1 ) ]
                                              , [ AST.PackageWord emptyRange [ "package", "module" ] "when-one"
                                                ]
                                              )
                                            ]
                                            [ AST.PackageWord emptyRange [ "package", "module" ] "when-other-one" ]
                                  }
                                , { name = "main"
                                  , typeSignature = AST.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "ali", "internal/alias" ) ]
                                  , imports = Dict.fromList [ ( "/list/of/names", [ "one" ] ) ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.PackageWord emptyRange [ "html" ] "div"
                                            , AST.Word emptyRange "call-external"
                                            , AST.ExternalWord emptyRange [ "some", "ext" ] "word"
                                            , AST.PackageWord emptyRange [ "ali" ] "word1"
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
                            , "/package/test/internal/alias"
                            , "/package/test/package/module"
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
                                    ]
                            }
                in
                Expect.equal expectedRequiredModules actualRequiredModules
        , describe "Module resolution" <|
            let
                dummyWord name =
                    ( name
                    , { name = name
                      , metadata = Metadata.default
                      , implementation =
                            SoloImpl []
                      }
                    )

                dummyWordUnexposed name =
                    ( name
                    , { name = name
                      , metadata =
                            Metadata.default
                                |> Metadata.isExposed False
                      , implementation =
                            SoloImpl []
                      }
                    )

                dummyType name =
                    let
                        genericName =
                            name ++ "Generic"

                        unionName =
                            name ++ "Union"
                    in
                    [ ( name
                      , CustomTypeDef name emptyRange [] []
                      )
                    , ( genericName
                      , CustomTypeDef genericName emptyRange [ "a" ] []
                      )
                    , ( unionName
                      , UnionTypeDef unionName
                            emptyRange
                            []
                            [ Type.Custom name
                            , Type.CustomGeneric genericName [ Type.Generic "a" ]
                            ]
                      )
                    ]
            in
            [ test "Qualifies word in internal package" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.PackageWord emptyRange [ "internal" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "internal/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "internal/add" ]
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) []
                                                  , [ AST.Integer emptyRange 0
                                                    , AST.PackageWord emptyRange [ "mod" ] "add"
                                                    ]
                                                  )
                                                ]
                                                [ AST.Integer emptyRange 1
                                                , AST.PackageWord emptyRange [ "mod" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int []
                                                  , [ Integer emptyRange 0
                                                    , Word emptyRange "mod/add"
                                                    ]
                                                  )
                                                ]
                                                [ Integer emptyRange 1
                                                , Word emptyRange "mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "mod/add" ]
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input =
                                                    [ AST.InternalRef [ "mod" ] "Tipe" []
                                                    , AST.InternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]
                                                    , AST.InternalRef [ "mod" ] "TipeUnion" []
                                                    ]
                                                , output = []
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Custom "mod/Tipe"
                                                    , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                    , Type.Union
                                                        [ Type.Custom "mod/Tipe"
                                                        , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                        ]
                                                    ]
                                                    []
                                      , implementation =
                                            SoloImpl
                                                [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input = [ AST.InternalRef [ "mod" ] "TipeUnion" [] ]
                                                , output = []
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.InternalRef [ "mod" ] "Tipe" []) []
                                                  , [ AST.Word emptyRange "drop"
                                                    , AST.Word emptyRange "drop"
                                                    ]
                                                  )
                                                , ( AST.TypeMatch emptyRange (AST.InternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]) []
                                                  , [ AST.Word emptyRange "drop"
                                                    , AST.Word emptyRange "drop"
                                                    ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Union
                                                        [ Type.Custom "mod/Tipe"
                                                        , Type.CustomGeneric "mod/TipeGeneric" [ Type.Generic "a" ]
                                                        ]
                                                    ]
                                                    []
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
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromList
                                    [ ( "SomeType"
                                      , AST.CustomTypeDef emptyRange
                                            "SomeType"
                                            []
                                            [ ( "tipe", AST.InternalRef [ "mod" ] "Tipe" [] ) ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        expectedAst =
                            { types =
                                Dict.fromList
                                    [ ( "SomeType"
                                      , CustomTypeDef "SomeType"
                                            emptyRange
                                            []
                                            [ ( "tipe", Type.Custom "mod/Tipe" ) ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.ExternalWord emptyRange [ "mod" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add" ]
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.LocalRef "Int" []) []
                                                  , [ AST.Integer emptyRange 0
                                                    , AST.ExternalWord emptyRange [ "mod" ] "add"
                                                    ]
                                                  )
                                                ]
                                                [ AST.Integer emptyRange 1
                                                , AST.ExternalWord emptyRange [ "mod" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange Type.Int []
                                                  , [ Integer emptyRange 0
                                                    , Word emptyRange "/external/package/mod/add"
                                                    ]
                                                  )
                                                ]
                                                [ Integer emptyRange 1
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add" ]
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input =
                                                    [ AST.ExternalRef [ "mod" ] "Tipe" []
                                                    , AST.ExternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]
                                                    , AST.ExternalRef [ "mod" ] "TipeUnion" []
                                                    ]
                                                , output = []
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Custom "/external/package/mod/Tipe"
                                                    , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                    , Type.Union
                                                        [ Type.Custom "/external/package/mod/Tipe"
                                                        , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                        ]
                                                    ]
                                                    []
                                      , implementation =
                                            SoloImpl
                                                [ Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                , Builtin emptyRange Builtin.StackDrop
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input = [ AST.ExternalRef [ "mod" ] "TipeUnion" [] ]
                                                , output = []
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.ExternalRef [ "mod" ] "Tipe" []) []
                                                  , [ AST.Word emptyRange "drop"
                                                    , AST.Word emptyRange "drop"
                                                    ]
                                                  )
                                                , ( AST.TypeMatch emptyRange (AST.ExternalRef [ "mod" ] "TipeGeneric" [ AST.Generic "a" ]) []
                                                  , [ AST.Word emptyRange "drop"
                                                    , AST.Word emptyRange "drop"
                                                    ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "drop"
                                                , AST.Word emptyRange "drop"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Union
                                                        [ Type.Custom "/external/package/mod/Tipe"
                                                        , Type.CustomGeneric "/external/package/mod/TipeGeneric" [ Type.Generic "a" ]
                                                        ]
                                                    ]
                                                    []
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
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromList
                                    [ ( "SomeType"
                                      , AST.CustomTypeDef emptyRange
                                            "SomeType"
                                            []
                                            [ ( "tipe", AST.ExternalRef [ "mod" ] "Tipe" [] ) ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        expectedAst =
                            { types =
                                Dict.fromList
                                    [ ( "SomeType"
                                      , CustomTypeDef "SomeType"
                                            emptyRange
                                            []
                                            [ ( "tipe", Type.Custom "/external/package/mod/Tipe" ) ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe" ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases =
                                        Dict.fromList
                                            [ ( "ext", "/mod" )
                                            , ( "internal", "internal/mod" )
                                            ]
                                    , imports = Dict.empty
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input = [ AST.InternalRef [ "ext" ] "Tipe" [] ]
                                                , output = [ AST.InternalRef [ "internal" ] "Tope" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.PackageWord emptyRange [ "internal" ] "value"
                                                , AST.PackageWord emptyRange [ "ext" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
                                                |> Metadata.withType
                                                    [ Type.Custom "/external/package/mod/Tipe"
                                                    ]
                                                    [ Type.Custom "internal/mod/Tope" ]
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "internal/mod/value"
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe"
                                , dummyType "internal/mod/Tope"
                                ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add"
                                    , dummyWord "internal/mod/value"
                                    ]
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases =
                                        Dict.fromList
                                            [ ( "ext", "/mod" ) ]
                                    , imports = Dict.empty
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input = [ AST.InternalRef [ "ext" ] "Tipe" [] ]
                                                , output = [ AST.InternalRef [ "internal" ] "Tope" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.fromList [ ( "internal", "internal/mod" ) ]
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.PackageWord emptyRange [ "internal" ] "value"
                                                , AST.PackageWord emptyRange [ "ext" ] "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
                                                |> Metadata.withType
                                                    [ Type.Custom "/external/package/mod/Tipe"
                                                    ]
                                                    [ Type.Custom "internal/mod/Tope" ]
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "internal/mod/value"
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe"
                                , dummyType "internal/mod/Tope"
                                ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add"
                                    , dummyWord "internal/mod/value"
                                    ]
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases =
                                        Dict.fromList
                                            [ ( "ext", "/mod" ) ]
                                    , imports = Dict.empty
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.fromList [ ( "internal", "internal/mod" ) ]
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (AST.InternalRef [ "ext" ] "Tipe" []) []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                , ( AST.TypeMatch emptyRange (AST.InternalRef [ "internal" ] "Tope" []) []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "drop" ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
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
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe"
                                , dummyType "internal/mod/Tope"
                                ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases =
                                        Dict.fromList
                                            [ ( "ext", "/mod" )
                                            , ( "mod", "internal/mod" )
                                            ]
                                    , imports = Dict.empty
                                    , exposes = Set.empty
                                    }
                            , types =
                                Dict.fromList
                                    [ ( "Tepip"
                                      , AST.CustomTypeDef
                                            emptyRange
                                            "Tepip"
                                            []
                                            [ ( "first", AST.InternalRef [ "ext" ] "Tipe" [] )
                                            , ( "second", AST.InternalRef [ "mod" ] "Tope" [] )
                                            ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        expectedAst =
                            { types =
                                Dict.fromList
                                    [ ( "Tepip"
                                      , CustomTypeDef
                                            "Tepip"
                                            emptyRange
                                            []
                                            [ ( "first", Type.Custom "/external/package/mod/Tipe" )
                                            , ( "second", Type.Custom "internal/mod/Tope" )
                                            ]
                                      )
                                    ]
                            , words = Dict.empty
                            }

                        inProgressAst =
                            { types =
                                [ dummyType "/external/package/mod/Tipe"
                                , dummyType "internal/mod/Tope"
                                ]
                                    |> List.concat
                                    |> Dict.fromList
                            , words = Dict.empty
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases = Dict.empty
                                    , imports =
                                        Dict.fromList
                                            [ ( "/mod", [ "add" ] )
                                            , ( "internal/mod", [] )
                                            ]
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "value"
                                                , AST.Word emptyRange "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "internal/mod/value"
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add"
                                    , dummyWord "internal/mod/value"
                                    ]
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
                            { moduleDefinition =
                                AST.Defined
                                    { aliases = Dict.empty
                                    , imports =
                                        Dict.fromList
                                            [ ( "internal/mod", [] )
                                            ]
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.fromList [ ( "/mod", [ "add" ] ) ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "value"
                                                , AST.Word emptyRange "add"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Word emptyRange "internal/mod/value"
                                                , Word emptyRange "/external/package/mod/add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWord "/external/package/mod/add"
                                    , dummyWord "internal/mod/value"
                                    ]
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
                            { moduleDefinition = AST.Undefined
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "fn1"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Integer emptyRange 2
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "fn2"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 2
                                                , AST.Integer emptyRange 3
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "fn1"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Integer emptyRange 2
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "fn2"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Integer emptyRange 3
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            , test "When module does have a definition, only functions defined to be exposed are" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { moduleDefinition =
                                AST.Defined
                                    { aliases = Dict.empty
                                    , imports = Dict.empty
                                    , exposes = Set.fromList [ "fn2" ]
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "fn1"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Integer emptyRange 2
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "fn2"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 2
                                                , AST.Integer emptyRange 3
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "fn1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isExposed False
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 1
                                                , Integer emptyRange 2
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "fn2"
                                      , metadata = Metadata.default
                                      , implementation =
                                            SoloImpl
                                                [ Integer emptyRange 2
                                                , Integer emptyRange 3
                                                , Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    QualifierUtil.expectOutput unqualifiedAst expectedAst
            , test "Referencing a function from an internal module which isn't exposed ends in a error" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { moduleDefinition =
                                AST.Defined
                                    { aliases = Dict.empty
                                    , imports =
                                        Dict.fromList
                                            [ ( "internal/mod", [] )
                                            ]
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "value"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWordUnexposed "internal/mod/value"
                                    ]
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

                        Err [ Problem.WordNotExposed _ "internal/mod/value" ] ->
                            Expect.pass

                        Err errs ->
                            Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
            , test "Referencing a function from an external module which isn't exposed ends in a error" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { moduleDefinition =
                                AST.Defined
                                    { aliases = Dict.empty
                                    , imports =
                                        Dict.fromList
                                            [ ( "/mod", [ "add" ] )
                                            ]
                                    , exposes = Set.empty
                                    }
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "external-call"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "add"
                                                ]
                                      }
                                    ]
                            }

                        inProgressAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromList
                                    [ dummyWordUnexposed "/external/package/mod/add"
                                    ]
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

                        Err [ Problem.WordNotExposed _ "/external/package/mod/add" ] ->
                            Expect.pass

                        Err errs ->
                            Expect.fail <| "Qualification failed with unexpected error: " ++ Debug.toString errs
            ]
        ]
