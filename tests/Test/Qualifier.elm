module Test.Qualifier exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.SourceLocation as PSourceLoc
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util as ParserUtil
import Test.Qualifier.Util as QualifierUtil


suite : Test
suite =
    describe "Qualifier"
        [ test "Name mangling" <|
            \_ ->
                let
                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    qualifiedUsMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            qualifiedUsMoneyUnion

                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "USMoney"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "Dollar" []
                                            , AST.LocalRef "Cent" []
                                            ]
                                  }
                                , { name = "Dollar"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                  }
                                , { name = "Cent"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "into-cents"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch PSourceLoc.emptyRange (AST.LocalRef "Dollar" []) []
                                              , [ AST.Function PSourceLoc.emptyRange "dollar-value>"
                                                , AST.Integer PSourceLoc.emptyRange 100
                                                , AST.Function PSourceLoc.emptyRange "*"
                                                ]
                                              )
                                            , ( AST.TypeMatch PSourceLoc.emptyRange (AST.LocalRef "Cent" []) []
                                              , [ AST.Function PSourceLoc.emptyRange "cent-value>"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "add-money"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "USMoney" [], AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "USMoney" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function PSourceLoc.emptyRange "into-cents"
                                            , AST.Function PSourceLoc.emptyRange "swap"
                                            , AST.Function PSourceLoc.emptyRange "into-cents"
                                            , AST.Function PSourceLoc.emptyRange "+"
                                            ]
                                  }
                                , { name = "quote-excuse"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.LocalRef "Dollar" [] ]
                                            , output = [ AST.NotStackRange <| AST.LocalRef "Dollar" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.Function PSourceLoc.emptyRange "dollar-value>"
                                            , AST.InlineFunction PSourceLoc.emptyRange
                                                [ AST.Integer PSourceLoc.emptyRange 2
                                                , AST.Function PSourceLoc.emptyRange "*"
                                                ]
                                            , AST.Function PSourceLoc.emptyRange "!"
                                            , AST.Function PSourceLoc.emptyRange ">Dollar"
                                            ]
                                  }
                                ]
                        }
                            |> ParserUtil.addFunctionsForStructs

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
                                ]
                        , referenceableFunctions =
                            Set.fromList
                                [ "inlinefn:/stabel/test/some/module/quote-excuse/1" ]
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , test "member types are qualified" <|
            \_ ->
                let
                    qualifiedUsMoneyUnion =
                        [ Type.Custom "/stabel/test/some/module/Dollar"
                        , Type.Custom "/stabel/test/some/module/Cent"
                        ]

                    qualifiedUsMoneyUnionType =
                        Type.Union (Just "/stabel/test/some/module/USMoney")
                            qualifiedUsMoneyUnion

                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "USMoney"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ AST.LocalRef "Dollar" []
                                            , AST.LocalRef "Cent" []
                                            ]
                                  }
                                , { name = "Wallet"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "user-id", AST.LocalRef "Int" [] )
                                            , ( "value", AST.LocalRef "USMoney" [] )
                                            ]
                                  }
                                , { name = "Dollar"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                  }
                                , { name = "Cent"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "cent-value", AST.LocalRef "Int" [] ) ]
                                  }
                                ]
                        , functions = Dict.empty
                        }
                            |> ParserUtil.addFunctionsForStructs

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
                                [ { name = "/stabel/test/some/module/>Dollar"
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
                        , referenceableFunctions = Set.empty
                        }
                in
                QualifierUtil.expectModuleOutput unqualifiedAst expectedAst
        , test "Retrieve dependant modules" <|
            \_ ->
                let
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition =
                            ModuleDefinition.Defined
                                { aliases =
                                    Dict.fromList
                                        [ ( "html", "/external/html" ) ]
                                , imports =
                                    Dict.fromList
                                        [ ( "/external/module", [] ) ]
                                , exposes = Set.fromList []
                                }
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Tipe"
                                  , sourceLocation = PSourceLoc.emptyRange
                                  , generics = []
                                  , members =
                                        AST.StructMembers
                                            [ ( "value", AST.ExternalRef [ "external", "double" ] "Tipe" [] ) ]
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "call-external"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ AST.NotStackRange <| AST.InternalRef [ "internal", "types" ] "In" [] ]
                                            , output = [ AST.NotStackRange <| AST.ExternalRef [ "external", "types" ] "Out" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.MultiImpl
                                            [ ( AST.TypeMatch PSourceLoc.emptyRange (AST.LocalRef "Int" []) [ ( "value", AST.LiteralInt 1 ) ]
                                              , [ AST.PackageFunction PSourceLoc.emptyRange [ "package", "module" ] "when-one"
                                                ]
                                              )
                                            , ( AST.TypeMatch PSourceLoc.emptyRange (AST.InternalRef [ "internal", "match" ] "Some" []) []
                                              , [ AST.Function PSourceLoc.emptyRange "drop" ]
                                              )
                                            ]
                                            [ AST.PackageFunction PSourceLoc.emptyRange [ "package", "module" ] "when-other-one" ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "ali", "internal/alias" ) ]
                                  , imports = Dict.fromList [ ( "/list/of/names", [ "one" ] ) ]
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.PackageFunction PSourceLoc.emptyRange [ "html" ] "div"
                                            , AST.Function PSourceLoc.emptyRange "call-external"
                                            , AST.ExternalFunction PSourceLoc.emptyRange [ "some", "ext" ] "word"
                                            , AST.PackageFunction PSourceLoc.emptyRange [ "ali" ] "word1"
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
                            , "/robheghan/html/external/types"
                            , "/robheghan/html/external/double"
                            , "/package/test/internal/alias"
                            , "/package/test/internal/types"
                            , "/package/test/internal/match"
                            , "/package/test/package/module"
                            , "/stabel/standard_library/core"
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
                    unqualifiedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.fromList [ ( "ali", "internal/alias" ) ]
                                  , imports = Dict.empty
                                  , implementation =
                                        AST.SoloImpl
                                            [ AST.PackageFunction PSourceLoc.emptyRange [ "ali" ] "word1"
                                            ]
                                  }
                                ]
                        }

                    expectedRequiredModules =
                        Set.fromList
                            [ "/package/test/internal/alias"
                            ]

                    actualRequiredModules =
                        requiredModules
                            { packageName = "package/test"
                            , ast = unqualifiedAst
                            , externalModules = Dict.empty
                            }
                in
                Expect.equal expectedRequiredModules actualRequiredModules
        ]
