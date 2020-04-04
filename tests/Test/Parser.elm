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

                        -- as int
                        , Metadata "def"
                        , Symbol "as-int"
                        , Metadata "type"
                        , Type "True"
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Token.Integer 1
                        ]

                    expectedAst =
                        { types =
                            Dict.fromList
                                [ ( "True"
                                  , { name = "True"
                                    , members = []
                                    }
                                  )
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
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Custom data structure with fields" <|
            \_ ->
                let
                    source =
                        [ Metadata "deftype"
                        , Type "Person"
                        , Metadata ""
                        , ListStart
                        , Metadata "age"
                        , Type "Int"
                        , Metadata "jobs"
                        , Type "Int"
                        , ListEnd

                        -- get-age
                        , Metadata "def"
                        , Symbol "get-age"
                        , Metadata "type"
                        , Type "Person"
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Token.Symbol "age>"
                        ]

                    expectedAst =
                        { types =
                            Dict.fromList
                                [ ( "Person"
                                  , { name = "Person"
                                    , members =
                                        [ ( "age", Type.Int )
                                        , ( "jobs", Type.Int )
                                        ]
                                    }
                                  )
                                ]
                        , words =
                            Dict.fromList
                                [ ( ">Person"
                                  , { name = ">Person"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int, Type.Int ] [ Type.Custom "Person" ]
                                    , implementation = [ AST.ConstructType "Person" ]
                                    }
                                  )
                                , ( ">age"
                                  , { name = ">age"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                    , implementation = [ AST.SetMember "Person" "age" ]
                                    }
                                  )
                                , ( ">jobs"
                                  , { name = ">jobs"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                    , implementation = [ AST.SetMember "Person" "jobs" ]
                                    }
                                  )
                                , ( "age>"
                                  , { name = "age>"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                    , implementation = [ AST.GetMember "Person" "age" ]
                                    }
                                  )
                                , ( "jobs>"
                                  , { name = "jobs>"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                    , implementation = [ AST.GetMember "Person" "jobs" ]
                                    }
                                  )
                                , ( "get-age"
                                  , { name = "get-age"
                                    , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                    , implementation =
                                        [ AST.Word "age>"
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
