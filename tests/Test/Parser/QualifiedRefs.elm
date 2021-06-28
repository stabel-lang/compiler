module Test.Parser.QualifiedRefs exposing (..)

import Dict
import Dict.Extra as Dict
import Stabel.Parser as AST exposing (..)
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.SourceLocation exposing (emptyRange)
import Stabel.Parser.Type exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (expectAst)


suite : Test
suite =
    describe "Parser -- QualifiedRefs"
        [ test "Internal function reference" <|
            \_ ->
                let
                    source =
                        """
                        def: test
                        : some/module/sample
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
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.PackageFunction emptyRange [ "some", "module" ] "sample" ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "External function reference" <|
            \_ ->
                let
                    source =
                        """
                        def: test
                        : /some/module/sample
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
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.ExternalFunction emptyRange [ "some", "module" ] "sample" ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "Internal _and_ external function reference" <|
            \_ ->
                let
                    source =
                        """
                        def: test
                        : internal/sample /some/module/sample
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
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.PackageFunction emptyRange [ "internal" ] "sample"
                                            , AST.ExternalFunction emptyRange [ "some", "module" ] "sample"
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "Internal types in type signature" <|
            \_ ->
                let
                    source =
                        """
                        def: test
                        type: internal/Tipe -- Int
                        : drop 1
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "test"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| InternalRef [ "internal" ] "Tipe" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "drop"
                                            , AST.Integer emptyRange 1
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "External types in type signature" <|
            \_ ->
                let
                    source =
                        """
                        def: test
                        type: /external/Tipe -- Int
                        : drop 1
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "test"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| ExternalRef [ "external" ] "Tipe" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "drop"
                                            , AST.Integer emptyRange 1
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "External type in multifn" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: test
                        type: /external/Tipe -- Int
                        : /external/Tipe( value 1 )
                          drop 1
                        else:
                          drop 0
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "test"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| ExternalRef [ "external" ] "Tipe" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (ExternalRef [ "external" ] "Tipe" []) [ ( "value", LiteralInt 1 ) ]
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Integer emptyRange 1
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
                                            , AST.Integer emptyRange 0
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "Internal type in multifn" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: test
                        type: internal/Tipe -- Int
                        : internal/Tipe( value 1 )
                          drop 1
                        else:
                          drop 0
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "test"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| InternalRef [ "internal" ] "Tipe" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch emptyRange (InternalRef [ "internal" ] "Tipe" []) [ ( "value", LiteralInt 1 ) ]
                                              , [ AST.Function emptyRange "drop"
                                                , AST.Integer emptyRange 1
                                                ]
                                              )
                                            ]
                                            [ AST.Function emptyRange "drop"
                                            , AST.Integer emptyRange 0
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        ]
