module Test.Qualifier.Error exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Stabel.Data.Metadata as Metadata
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Parser as AST
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem exposing (Problem(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier errors"
        [ describe "No such reference" <|
            [ test "Word" <|
                \_ ->
                    let
                        ast =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "inc"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
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
                    in
                    checkForError (noSuchWordReferenceError "dec") ast
            , test "External" <|
                \_ ->
                    let
                        ast =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "main"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "/external/module/inc"
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "inc"
                                      , typeSignature =
                                            AST.UserProvided
                                                { input = [ AST.LocalRef "Ints" [] ]
                                                , output = [ AST.LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , typeSignature = AST.NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "inc"
                                                , AST.Integer emptyRange 2
                                                , AST.Word emptyRange "="
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
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.UnionTypeDef
                                        emptyRange
                                        "USMoney"
                                        []
                                        [ AST.LocalRef "Dollar" []
                                        , AST.LocalRef "Cent" []
                                        ]
                                    , AST.CustomTypeDef
                                        emptyRange
                                        "Dollar"
                                        []
                                        [ ( "dollar-value", AST.LocalRef "Int" [] ) ]
                                    ]
                            , words = Dict.empty
                            }
                    in
                    checkForError (noSuchTypeReferenceError "Cent") ast
            , test "Wrong reference within custom type definition" <|
                \_ ->
                    let
                        ast =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ AST.CustomTypeDef
                                        emptyRange
                                        "BoxWrapper"
                                        []
                                        [ ( "box", AST.LocalRef "Box" [] ) ]
                                    ]
                            , words = Dict.empty
                            }
                    in
                    checkForError (noSuchTypeReferenceError "Box") ast
            ]
        ]


noSuchWordReferenceError : String -> Problem -> Bool
noSuchWordReferenceError name problem =
    case problem of
        UnknownWordRef _ problemName ->
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
                    , words = Dict.empty
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
