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
import Test exposing (Test, describe, test)


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
                                  , sourceLocation = Nothing
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , sourceLocation = Nothing
                                  , metadata = Metadata.default
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , sourceLocation = Nothing
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
                                            [ Integer 1
                                            , Builtin Builtin.Plus
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ Integer 1
                                            , Builtin Builtin.Minus
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Integer 1
                                            , Word "inc"
                                            , Word "inc"
                                            , Word "dec"
                                            , Integer 2
                                            , Builtin Builtin.Equal
                                            ]
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        , test "Generic function types" <|
            \_ ->
                let
                    unqualifiedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , sourceLocation = Nothing
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
                                            [ Builtin Builtin.StackSwap
                                            , Builtin Builtin.StackDuplicate
                                            , Builtin Builtin.StackRightRotate
                                            ]
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
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
                                [ { name = ">True"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "False" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "False"
                                            ]
                                  }
                                , { name = "to-int"
                                  , sourceLocation = Nothing
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

                    expectedAst =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" [] []
                                , CustomTypeDef "False" [] []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
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
                                , { name = "to-int"
                                  , metadata = Metadata.default
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch (Type.Custom "False") [], [ Integer 0 ] )
                                            , ( TypeMatch (Type.Custom "True") [], [ Integer 1 ] )
                                            ]
                                            []
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        , describe "Quotations"
            [ test "Basic case" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "apply-to-num"
                                      , sourceLocation = Nothing
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
                                      , sourceLocation = Nothing
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
                                                [ Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , WordRef "main__quote2"
                                                , Word "apply-to-num"
                                                , WordRef "main__quote1"
                                                , Word "apply-to-num"
                                                ]
                                      }
                                    , { name = "main__quote2"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quote1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Minus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case qualify unqualifiedAst of
                        Err () ->
                            Expect.fail "Did not expect qualification to fail"

                        Ok qualifiedAst ->
                            Expect.equal expectedAst qualifiedAst
            , test "Do not create new function if quoting exactly one word" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
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
                                      , sourceLocation = Nothing
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
                                                [ Integer 1
                                                , WordRef "inc"
                                                , Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "inc"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case qualify unqualifiedAst of
                        Err () ->
                            Expect.fail "Did not expect qualification to fail"

                        Ok qualifiedAst ->
                            Expect.equal expectedAst qualifiedAst
            , test "Quotes within quotes is fine" <|
                \_ ->
                    let
                        unqualifiedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , sourceLocation = Nothing
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
                                                [ Integer 1
                                                , WordRef "main__quote1"
                                                , Builtin Builtin.Apply
                                                ]
                                      }
                                    , { name = "main__quote1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , WordRef "main__quote1__quote1"
                                                , Builtin Builtin.Apply
                                                , Builtin Builtin.Plus
                                                ]
                                      }
                                    , { name = "main__quote1__quote1"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.isQuoted
                                      , implementation =
                                            SoloImpl
                                                [ Integer 1
                                                , Builtin Builtin.Plus
                                                ]
                                      }
                                    ]
                            }
                    in
                    case qualify unqualifiedAst of
                        Err () ->
                            Expect.fail "Did not expect qualification to fail"

                        Ok qualifiedAst ->
                            Expect.equal expectedAst qualifiedAst
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
                                    [ { name = ">True"
                                      , sourceLocation = Nothing
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "True" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "True"
                                                ]
                                      }
                                    , { name = ">False"
                                      , sourceLocation = Nothing
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "False" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "False"
                                                ]
                                      }
                                    , { name = ">Box"
                                      , sourceLocation = Nothing
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Box" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">value"
                                      , sourceLocation = Nothing
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int, Type.Custom "Box" ] [ Type.Custom "Box" ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.SetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "<value"
                                      , sourceLocation = Nothing
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Box" ] [ Type.Int ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.GetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "zero?"
                                      , sourceLocation = Nothing
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.MultiImpl
                                                [ ( AST.TypeMatch emptyRange (Type.Custom "Box") [ ( "value", AST.LiteralInt 0 ) ]
                                                  , [ AST.Word emptyRange ">True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange ">False" ]
                                      }
                                    ]
                            }

                        expectedAst =
                            { types =
                                Dict.fromListBy typeDefinitionName
                                    [ UnionTypeDef "Bool"
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , CustomTypeDef "True" [] []
                                    , CustomTypeDef "False" [] []
                                    , CustomTypeDef "Box"
                                        []
                                        [ ( "value", Type.Int ) ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">True"
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
                                    , { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int ] [ Type.Custom "Box" ]
                                      , implementation =
                                            SoloImpl
                                                [ ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int, Type.Custom "Box" ] [ Type.Custom "Box" ]
                                      , implementation =
                                            SoloImpl
                                                [ SetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "<value"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Box" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ GetMember "Box" "value"
                                                ]
                                      }
                                    , { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.Custom "Box") [ ( "value", LiteralInt 0 ) ], [ Word ">True" ] )
                                                ]
                                                [ Word ">False" ]
                                      }
                                    ]
                            }
                    in
                    case qualify unqualifiedAst of
                        Err () ->
                            Expect.fail "Did not expect qualification to fail"

                        Ok qualifiedAst ->
                            Expect.equal expectedAst qualifiedAst
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
                                [ { name = ">True"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "True" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "False" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "False"
                                            ]
                                  }
                                , { name = ">Box"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "Bool" ] [ Type.Custom "Box" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.ConstructType "Box"
                                            ]
                                  }
                                , { name = ">value"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType
                                                [ Type.Custom "Bool", Type.Custom "Box" ]
                                                [ Type.Custom "Box" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.SetMember "Box" "value"
                                            ]
                                  }
                                , { name = "<value"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "Box" ] [ Type.Custom "Bool" ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.GetMember "Box" "value"
                                            ]
                                  }
                                , { name = "true?"
                                  , sourceLocation = Nothing
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Box" ] [ Type.Custom "Bool" ]
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch emptyRange
                                                    (Type.Custom "Box")
                                                    [ ( "value", AST.LiteralType (Type.Custom "True") ) ]
                                              , [ AST.Word emptyRange ">True" ]
                                              )
                                            ]
                                            [ AST.Word emptyRange ">False" ]
                                  }
                                ]
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" [] []
                                , CustomTypeDef "False" [] []
                                , CustomTypeDef "Box"
                                    []
                                    [ ( "value", boolUnion ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "True" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "True"
                                            ]
                                  }
                                , { name = ">False"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "False" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "False"
                                            ]
                                  }
                                , { name = ">Box"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ boolUnion ] [ Type.Custom "Box" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "Box"
                                            ]
                                  }
                                , { name = ">value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ boolUnion, Type.Custom "Box" ] [ Type.Custom "Box" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "Box" "value"
                                            ]
                                  }
                                , { name = "<value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "Box" ] [ boolUnion ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "Box" "value"
                                            ]
                                  }
                                , { name = "true?"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Box" ] [ boolUnion ]
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch (Type.Custom "Box")
                                                    [ ( "value", LiteralType (Type.Custom "True") ) ]
                                              , [ Word ">True" ]
                                              )
                                            ]
                                            [ Word ">False" ]
                                  }
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        ]
