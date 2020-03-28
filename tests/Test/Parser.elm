module Test.Parser exposing (..)

import Dict
import Expect
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type
import Play.Parser as AST exposing (..)
import Play.Tokenizer as Token exposing (Token(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser"
        [ test "Sample program" <|
            \_ ->
                let
                    source =
                        [ -- inc function
                          Metadata "def"
                        , Symbol "inc"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "+"

                        -- dec function
                        , Metadata "def"
                        , Symbol "dec"
                        , Metadata "type"
                        , Type "Int"
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "-"
                        , Metadata "def"

                        -- main function
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Token.Integer 1
                        , Symbol "inc"
                        , Symbol "inc"
                        , Symbol "dec"
                        , Token.Integer 2
                        , Symbol "="
                        ]

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "inc"
                                  , { name = "inc"
                                    , metadata = Metadata.default
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "+"
                                        ]
                                    }
                                  )
                                , ( "dec"
                                  , { name = "dec"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "-"
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "inc"
                                        , AST.Word "inc"
                                        , AST.Word "dec"
                                        , AST.Integer 2
                                        , AST.Word "="
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Custom data structure without fields" <|
            \_ ->
                let
                    source =
                        [ Metadata "deftype"
                        , Type "True"
                        , Metadata "def"

                        -- as int
                        , Symbol "as-int"
                        , Metadata "type"
                        , Type "True"
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Token.Integer 1

                        -- entry point
                        , Metadata "def"
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Symbol ">True"
                        , Symbol "as-int"
                        ]

                    expectedAst =
                        { types =
                            Dict.fromList
                                [ ( "True", { name = "True" } )
                                ]
                        , words =
                            Dict.fromList
                                [ ( ">True"
                                  , { name = ">True"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                    , implementation = [ AST.ConstructType "True" ]
                                    }
                                  )
                                , ( "as-int"
                                  , { name = "as-int"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "True" ] [ Type.Int ]
                                    , implementation =
                                        [ AST.Integer 1
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                    , implementation =
                                        [ AST.Word ">True"
                                        , AST.Word "as-int"
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        ]
