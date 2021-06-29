module Test.Qualifier.Errors exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Stabel.Parser as AST
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Type as AST
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem exposing (Problem(..))
import Stabel.Qualifier.SourceLocation exposing (emptyRange)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier -- Errors"
        [ describe "No such reference" <|
            [ test "Word" <|
                \_ ->
                    let
                        ast =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "inc"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "inc"
                                                , AST.Function emptyRange "inc"
                                                , AST.Function emptyRange "dec"
                                                , AST.Integer emptyRange 2
                                                , AST.Function emptyRange "="
                                                ]
                                      }
                                    ]
                            }
                    in
                    checkForError (noSuchWordReferenceError "dec") ast
            , test "External" <|
                \_ ->
                    let
                        ast =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "/external/module/inc"
                                                ]
                                      }
                                    ]
                            }
                    in
                    checkForError (noSuchWordReferenceError "/external/module/inc") ast
            , test "Type" <|
                \_ ->
                    let
                        ast =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "inc"
                                      , typeSignature =
                                            AssociatedFunctionSignature.UserProvided
                                                { input = [ AST.NotStackRange <| AST.LocalRef "Ints" [] ]
                                                , output = [ AST.NotStackRange <| AST.LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "inc"
                                                , AST.Integer emptyRange 2
                                                , AST.Function emptyRange "="
                                                ]
                                      }
                                    ]
                            }
                    in
                    checkForError (noSuchTypeReferenceError "Ints") ast
            , test "Wrong reference within union definition" <|
                \_ ->
                    let
                        ast =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "USMoney"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            AST.UnionMembers
                                                [ AST.LocalRef "Dollar" []
                                                , AST.LocalRef "Cent" []
                                                ]
                                      }
                                    , { name = "Dollar"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            AST.StructMembers
                                                [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                      }
                                    ]
                            , functions = Dict.empty
                            }
                    in
                    checkForError (noSuchTypeReferenceError "Cent") ast
            , test "Wrong reference within custom type definition" <|
                \_ ->
                    let
                        ast =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "BoxWrapper"
                                      , sourceLocation = emptyRange
                                      , generics = []
                                      , members =
                                            AST.StructMembers
                                                [ ( "box", AST.LocalRef "Box" [] ) ]
                                      }
                                    ]
                            , functions = Dict.empty
                            }
                    in
                    checkForError (noSuchTypeReferenceError "Box") ast
            ]
        ]


noSuchWordReferenceError : String -> Problem -> Bool
noSuchWordReferenceError name problem =
    case problem of
        UnknownFunctionRef _ problemName ->
            name == problemName

        _ ->
            False


noSuchTypeReferenceError : String -> Problem -> Bool
noSuchTypeReferenceError name problem =
    case problem of
        UnknownTypeRef _ problemName ->
            name == problemName

        _ ->
            False


checkForError : (Problem -> Bool) -> AST.AST -> Expectation
checkForError fn source =
    let
        result =
            run
                { packageName = ""
                , modulePath = ""
                , ast = source
                , externalModules = Dict.empty
                , inProgressAST =
                    { types = Dict.empty
                    , functions = Dict.empty
                    }
                }
    in
    case result of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect parsing to succeed"
