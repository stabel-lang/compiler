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
        [ test "Recursive function" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: count-down
                        : Int( value 0 )
                          0
                        : Int
                          1 - count-down
                        """
                in
                QualifierUtil.expectQualification source
        , test "Recursive function through inline function" <|
            \_ ->
                let
                    source =
                        """
                        defmulti: count-down
                        : Int( value 0 )
                          0
                        : Int
                          [ count-down ] !
                        """
                in
                QualifierUtil.expectQualification source
        , test "Function cycle" <|
            \_ ->
                let
                    source =
                        """
                        def: dec-down
                        : 1 - count-down 

                        defmulti: count-down
                        : Int( value 0 )
                          0
                        : Int
                          dec-down
                        """
                in
                QualifierUtil.expectQualification source
        , test "Function cycle with inline function" <|
            \_ ->
                let
                    source =
                        """
                        def: dec-down
                        : 1 - count-down 

                        defmulti: count-down
                        : Int( value 0 )
                          0
                        : Int
                          [ dec-down ] !
                        """
                in
                QualifierUtil.expectQualification source
        , test "Name mangling" <|
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

                    usMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    usMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            usMoneyUnion

                    usMoneyTypeDef =
                        { name = "/stabel/test/some/module/USMoney"
                        , exposed = True
                        , sourceLocation = emptyRange
                        , generics = []
                        , members = UnionMembers usMoneyUnion
                        }

                    dollarTypeDef =
                        { name = "/stabel/test/some/module/Dollar"
                        , exposed = True
                        , sourceLocation = emptyRange
                        , generics = []
                        , members =
                            StructMembers
                                [ ( "dollar-value", Type.Int ) ]
                        }

                    centTypeDef =
                        { name = "/stabel/test/some/module/Cent"
                        , exposed = True
                        , sourceLocation = emptyRange
                        , generics = []
                        , members =
                            StructMembers
                                [ ( "cent-value", Type.Int ) ]
                        }

                    walletTypeDef =
                        { name = "/stabel/test/some/module/Wallet"
                        , exposed = True
                        , sourceLocation = emptyRange
                        , generics = []
                        , members =
                            StructMembers
                                [ ( "user-id", Type.Int )
                                , ( "value", usMoneyUnionType )
                                ]
                        }

                    intoCentsFn =
                        { name = "/stabel/test/some/module/into-cents"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.UserProvided
                                { input = [ usMoneyUnionType ]
                                , output = [ Type.Int ]
                                }
                        , implementation =
                            MultiImpl
                                [ ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Dollar") []
                                  , [ Function emptyRange dollarValueGetFn
                                    , Integer emptyRange 100
                                    , Builtin emptyRange Builtin.Multiply
                                    ]
                                  )
                                , ( TypeMatch emptyRange (Type.Custom "/stabel/test/some/module/Cent") []
                                  , [ Function emptyRange centValueGetFn
                                    ]
                                  )
                                ]
                                []
                        }

                    addMoneyFn =
                        { name = "/stabel/test/some/module/add-money"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.UserProvided
                                { input = [ usMoneyUnionType, usMoneyUnionType ]
                                , output = [ usMoneyUnionType ]
                                }
                        , implementation =
                            SoloImpl
                                [ Function emptyRange intoCentsFn
                                , Builtin emptyRange Builtin.StackSwap
                                , Function emptyRange intoCentsFn
                                , Builtin emptyRange Builtin.Plus
                                , Function emptyRange centCtorFn
                                ]
                        }

                    quoteExcuseFn =
                        { name = "/stabel/test/some/module/quote-excuse"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.UserProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                }
                        , implementation =
                            SoloImpl
                                [ Function emptyRange dollarValueGetFn
                                , FunctionRef emptyRange quoteExcuseIFn1
                                , Builtin emptyRange Builtin.Apply
                                , Function emptyRange dollarCtorFn
                                ]
                        }

                    quoteExcuseIFn1 =
                        { name = "inlinefn:/stabel/test/some/module/quote-excuse/1"
                        , exposed = False
                        , sourceLocation = Nothing
                        , typeSignature = TypeSignature.NotProvided
                        , implementation =
                            SoloImpl
                                [ Integer emptyRange 2
                                , Builtin emptyRange Builtin.Multiply
                                ]
                        }

                    dollarCtorFn =
                        { name = "/stabel/test/some/module/>Dollar"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Int ]
                                , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                }
                        , implementation =
                            SoloImpl [ ConstructType dollarTypeDef ]
                        }

                    centCtorFn =
                        { name = "/stabel/test/some/module/>Cent"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Int ]
                                , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                }
                        , implementation =
                            SoloImpl [ ConstructType centTypeDef ]
                        }

                    walletCtorFn =
                        { name = "/stabel/test/some/module/>Wallet"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Int, usMoneyUnionType ]
                                , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                }
                        , implementation =
                            SoloImpl [ ConstructType walletTypeDef ]
                        }

                    dollarValueSetFn =
                        { name = "/stabel/test/some/module/>dollar-value"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Dollar", Type.Int ]
                                , output = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                }
                        , implementation =
                            SoloImpl
                                [ SetMember dollarTypeDef "dollar-value" Type.Int ]
                        }

                    centValueSetFn =
                        { name = "/stabel/test/some/module/>cent-value"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Cent", Type.Int ]
                                , output = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                }
                        , implementation =
                            SoloImpl
                                [ SetMember centTypeDef "cent-value" Type.Int ]
                        }

                    userIdSetFn =
                        { name = "/stabel/test/some/module/>user-id"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Wallet", Type.Int ]
                                , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                }
                        , implementation =
                            SoloImpl
                                [ SetMember walletTypeDef "user-id" Type.Int ]
                        }

                    valueSetFn =
                        { name = "/stabel/test/some/module/>value"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Wallet", usMoneyUnionType ]
                                , output = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                }
                        , implementation =
                            SoloImpl
                                [ SetMember walletTypeDef "value" usMoneyUnionType ]
                        }

                    dollarValueGetFn =
                        { name = "/stabel/test/some/module/dollar-value>"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Dollar" ]
                                , output = [ Type.Int ]
                                }
                        , implementation =
                            SoloImpl
                                [ GetMember dollarTypeDef "dollar-value" Type.Int ]
                        }

                    centValueGetFn =
                        { name = "/stabel/test/some/module/cent-value>"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Cent" ]
                                , output = [ Type.Int ]
                                }
                        , implementation =
                            SoloImpl
                                [ GetMember centTypeDef "cent-value" Type.Int ]
                        }

                    userIdGetFn =
                        { name = "/stabel/test/some/module/user-id>"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                , output = [ Type.Int ]
                                }
                        , implementation =
                            SoloImpl
                                [ GetMember walletTypeDef "user-id" Type.Int ]
                        }

                    valueGetFn =
                        { name = "/stabel/test/some/module/value>"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature =
                            TypeSignature.CompilerProvided
                                { input = [ Type.Custom "/stabel/test/some/module/Wallet" ]
                                , output = [ usMoneyUnionType ]
                                }
                        , implementation =
                            SoloImpl
                                [ GetMember walletTypeDef "value" usMoneyUnionType ]
                        }

                    expectedAst =
                        { types =
                            Dict.fromListBy .name
                                [ usMoneyTypeDef
                                , dollarTypeDef
                                , centTypeDef
                                , walletTypeDef
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ intoCentsFn
                                , addMoneyFn
                                , quoteExcuseFn
                                , quoteExcuseIFn1
                                , dollarCtorFn
                                , centCtorFn
                                , walletCtorFn
                                , dollarValueSetFn
                                , centValueSetFn
                                , userIdSetFn
                                , valueSetFn
                                , dollarValueGetFn
                                , centValueGetFn
                                , userIdGetFn
                                , valueGetFn
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
                        else: package/module/when-other-one

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
