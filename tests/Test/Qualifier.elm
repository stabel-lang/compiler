module Test.Qualifier exposing (suite)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as Parser
import Stabel.Qualifier exposing (..)
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as QualifierUtil


suite : Test
suite =
    describe "Qualifier"
        [ test "Name mangling" <|
            \_ ->
                let
                    source =
                        """
                        defunion: USMoney
                        : Dollar
                        : Cent

                        defstruct: Dollar
                        : dollar-value Int

                        defstruct: Cent
                        : cent-value Int

                        defstruct: Wallet
                        : user-id Int
                        : value USMoney

                        defmulti: into-cents
                        type: USMoney -- Int
                        : Dollar
                          dollar-value>
                          100 *
                        : Cent
                          cent-value>

                        def: add-money
                        type: USMoney USMoney -- USMoney
                        : into-cents
                          swap into-cents
                          +
                          >Cent

                        def: quote-excuse
                        type: Dollar -- Dollar
                        : dollar-value>
                          [ 2 * ] !
                          >Dollar
                        """

                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    qualifiedUsMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            qualifiedUsMoneyUnion

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
                        , referenceableFunctions =
                            Set.fromList
                                [ "inlinefn:/stabel/test/some/module/quote-excuse/1" ]
                        }
                in
                QualifierUtil.expectModuleOutput source expectedAst
        , test "Retrieve dependant modules" <|
            \_ ->
                let
                    source =
                        """
                        defmodule:
                        alias: html /external/html
                        import: /external/module
                        :

                        defstruct: Tipe
                        : value /external/double/Tipe

                        defmulti: call-external
                        type: internal/types/In -- /external/types/Out
                        : Int( value 1 )
                          package/module/when-one
                        : internal/match/Some
                          drop
                        : package/module/when-other-one

                        def: main
                        alias: ali internal/alias
                        import: /list/of/names one
                        : html/div
                          call-external
                          /some/ext/word
                          ali/word1
                        """

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
                in
                case Parser.run "test" source of
                    Err errors ->
                        Expect.fail <| "Parser error: " ++ Debug.toString errors

                    Ok parserAst ->
                        let
                            actualRequiredModules =
                                requiredModules
                                    { packageName = "package/test"
                                    , ast = parserAst
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
                    source =
                        """
                        def: main
                        alias: ali internal/alias
                        : ali/word1
                        """

                    expectedRequiredModules =
                        Set.fromList
                            [ "/package/test/internal/alias"
                            ]
                in
                case Parser.run "test" source of
                    Err errors ->
                        Expect.fail <| "Parser error: " ++ Debug.toString errors

                    Ok parserAst ->
                        let
                            actualRequiredModules =
                                requiredModules
                                    { packageName = "package/test"
                                    , ast = parserAst
                                    , externalModules = Dict.empty
                                    }
                        in
                        Expect.equal expectedRequiredModules actualRequiredModules
        ]
