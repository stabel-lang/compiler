module Test.Parser exposing (..)

import Dict
import Dict.Extra as Dict
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
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "+"
                                        ]
                                  }
                                , { name = "dec"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "-"
                                        ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "inc"
                                        , AST.Word "inc"
                                        , AST.Word "dec"
                                        , AST.Integer 2
                                        , AST.Word "="
                                        ]
                                  }
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
                            Dict.fromListBy AST.typeDefinitionName
                                [ CustomTypeDef "True" []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , whens = []
                                  , implementation = [ AST.ConstructType "True" ]
                                  }
                                , { name = "as-int"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "True" ] [ Type.Int ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        ]
                                  }
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
                            Dict.fromListBy AST.typeDefinitionName
                                [ CustomTypeDef "Person"
                                    [ ( "age", Type.Int )
                                    , ( "jobs", Type.Int )
                                    ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">Person"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int, Type.Int ] [ Type.Custom "Person" ]
                                  , whens = []
                                  , implementation = [ AST.ConstructType "Person" ]
                                  }
                                , { name = ">age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                  , whens = []
                                  , implementation = [ AST.SetMember "Person" "age" ]
                                  }
                                , { name = ">jobs"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                  , whens = []
                                  , implementation = [ AST.SetMember "Person" "jobs" ]
                                  }
                                , { name = "age>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                  , whens = []
                                  , implementation = [ AST.GetMember "Person" "age" ]
                                  }
                                , { name = "jobs>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                  , whens = []
                                  , implementation = [ AST.GetMember "Person" "jobs" ]
                                  }
                                , { name = "get-age"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Word "age>"
                                        ]
                                  }
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Parser understands generic types" <|
            \_ ->
                let
                    source =
                        [ Metadata "def"
                        , Symbol "over"
                        , Metadata "type"
                        , Symbol "a"
                        , Symbol "b"
                        , TypeSeperator
                        , Symbol "a"
                        , Symbol "b"
                        , Symbol "a"
                        , Metadata ""
                        , Symbol "dup"
                        , Symbol "rotate"
                        ]

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "a", Type.Generic "b", Type.Generic "a" ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Word "dup"
                                        , AST.Word "rotate"
                                        ]
                                  }
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Parser understands union types and multifunctions" <|
            \_ ->
                let
                    source =
                        [ Metadata "defunion"
                        , Type "Bool"
                        , Metadata ""
                        , ListStart
                        , Type "True"
                        , Type "False"
                        , ListEnd

                        -- True
                        , Metadata "deftype"
                        , Type "True"

                        -- False
                        , Metadata "deftype"
                        , Type "False"

                        -- Multifn
                        , Metadata "defmulti"
                        , Symbol "to-int"
                        , Metadata "when"
                        , Type "True"
                        , Token.Integer 1
                        , Metadata "when"
                        , Type "False"
                        , Token.Integer 0
                        ]

                    expectedAst =
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ UnionTypeDef "Bool"
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef "True" []
                                , CustomTypeDef "False" []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "True" ]
                                  , whens = []
                                  , implementation =
                                        [ AST.ConstructType "True"
                                        ]
                                  }
                                , { name = ">False"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [] [ Type.Custom "False" ]
                                  , whens = []
                                  , implementation =
                                        [ AST.ConstructType "False"
                                        ]
                                  }
                                , { name = "to-int"
                                  , metadata = Metadata.default
                                  , whens =
                                        [ ( Type.Custom "False", [ AST.Integer 0 ] )
                                        , ( Type.Custom "True", [ AST.Integer 1 ] )
                                        ]
                                  , implementation = []
                                  }
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Parser understands quotations" <|
            \_ ->
                let
                    source =
                        [ Metadata "def"
                        , Symbol "apply-to-num"
                        , Metadata "type"
                        , Type "Int"
                        , QuoteStart
                        , Type "Int"
                        , TypeSeperator
                        , Type "Int"
                        , QuoteStop
                        , TypeSeperator
                        , Type "Int"
                        , Metadata ""
                        , Symbol "!"

                        -- main
                        , Metadata "def"
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Token.Integer 1
                        , QuoteStart
                        , Token.Integer 1
                        , Symbol "+"
                        , QuoteStop
                        , Symbol "apply-to-num"
                        ]

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
                                  , whens = []
                                  , implementation =
                                        [ AST.Word "!"
                                        ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Quotation
                                            [ AST.Integer 1
                                            , AST.Word "+"
                                            ]
                                        , AST.Word "apply-to-num"
                                        ]
                                  }
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Parser understands stack ranges" <|
            \_ ->
                let
                    source =
                        [ Metadata "def"
                        , Symbol "apply-to-num"
                        , Metadata "type"
                        , Symbol "a..."
                        , QuoteStart
                        , Symbol "a..."
                        , TypeSeperator
                        , Symbol "b..."
                        , QuoteStop
                        , TypeSeperator
                        , Symbol "b..."
                        , Metadata ""
                        , Symbol "!"

                        -- main
                        , Metadata "def"
                        , Symbol "main"
                        , Metadata "entry"
                        , Symbol "true"
                        , Metadata ""
                        , Token.Integer 1
                        , QuoteStart
                        , Token.Integer 1
                        , Symbol "+"
                        , QuoteStop
                        , Symbol "apply-to-num"
                        ]

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.StackRange "a"
                                                , Type.Quotation
                                                    { input = [ Type.StackRange "a" ]
                                                    , output = [ Type.StackRange "b" ]
                                                    }
                                                ]
                                                [ Type.StackRange "b" ]
                                  , whens = []
                                  , implementation =
                                        [ AST.Word "!"
                                        ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , whens = []
                                  , implementation =
                                        [ AST.Integer 1
                                        , AST.Quotation
                                            [ AST.Integer 1
                                            , AST.Word "+"
                                            ]
                                        , AST.Word "apply-to-num"
                                        ]
                                  }
                                ]
                        }
                in
                case parse source of
                    Err () ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        ]
