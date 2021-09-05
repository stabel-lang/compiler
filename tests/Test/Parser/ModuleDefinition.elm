module Test.Parser.ModuleDefinition exposing (..)

import Dict
import Dict.Extra as Dict
import Set
import Stabel.Parser as AST exposing (..)
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.SourceLocation exposing (emptyRange)
import Stabel.Parser.Type exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util
    exposing
        ( addFunctionsForStructs
        , expectAst
        )


suite : Test
suite =
    describe "Parser -- ModuleDefinition"
        [ test "Imports and aliases" <|
            \_ ->
                let
                    source =
                        """
                        defmodule:
                        alias: other /some/mod
                        alias: moar local/mod
                        import: /some/other/mod test1 word2
                        import: internals foo
                        import: internal/mod
                        exposing: inc
                        :

                        defstruct: Pair a b
                        : first a
                        : second b

                        def: inc
                        : 1 +
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "other", "/some/mod" )
                                        , ( "moar", "local/mod" )
                                        ]
                                , imports =
                                    Dict.fromList
                                        [ ( "/some/other/mod", [ "test1", "word2" ] )
                                        , ( "internals", [ "foo" ] )
                                        , ( "internal/mod", [] )
                                        ]
                                , exposes = Set.fromList [ "inc" ]
                                , documentation = ""
                                }
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Pair"
                                  , sourceLocation = emptyRange
                                  , documentation = ""
                                  , generics = [ "a", "b" ]
                                  , members =
                                        AST.StructMembers
                                            [ ( "first", Generic "a" )
                                            , ( "second", Generic "b" )
                                            ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                ]
                        }
                            |> addFunctionsForStructs
                in
                expectAst source expectedAst
        , test "Functions can have its own aliases and imports" <|
            \_ ->
                let
                    source =
                        """
                        def: inc
                        alias: other /some/mod
                        alias: moar local/mod
                        import: /some/other/mod test1 word2
                        import: internals foo
                        import: internal/mod
                        : 1 +
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases =
                                        Dict.fromList
                                            [ ( "other", "/some/mod" )
                                            , ( "moar", "local/mod" )
                                            ]
                                  , imports =
                                        Dict.fromList
                                            [ ( "/some/other/mod", [ "test1", "word2" ] )
                                            , ( "internals", [ "foo" ] )
                                            , ( "internal/mod", [] )
                                            ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                ]
                        }
                            |> addFunctionsForStructs
                in
                expectAst source expectedAst
        , test "Multifunctions can have its own aliases and imports" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: with-default
                        alias: other /some/mod
                        alias: moar local/mod
                        import: /some/other/mod test1 word2
                        import: internals foo
                        import: internal/mod
                        : a
                          drop
                        else:
                          swap drop
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "with-default"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases =
                                        Dict.fromList
                                            [ ( "other", "/some/mod" )
                                            , ( "moar", "local/mod" )
                                            ]
                                  , imports =
                                        Dict.fromList
                                            [ ( "/some/other/mod", [ "test1", "word2" ] )
                                            , ( "internals", [ "foo" ] )
                                            , ( "internal/mod", [] )
                                            ]
                                  , implementation =
                                        MultiImpl
                                            [ ( AST.TypeMatch emptyRange (Generic "a") []
                                              , [ AST.Function emptyRange "drop" ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "swap"
                                            , AST.Function emptyRange "drop"
                                            ]
                                  }
                                ]
                        }
                            |> addFunctionsForStructs
                in
                expectAst source expectedAst
        , test "external reference path can be single module" <|
            \_ ->
                let
                    source =
                        """
                            def: test
                            : /ext/fn1
                            """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "test"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.ExternalFunction emptyRange [ "ext" ] "fn1" ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        ]
