module Test.Qualifier.Error exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type
import Play.Parser as AST
import Play.Qualifier exposing (..)
import Play.Qualifier.Problem exposing (Problem(..))
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
                                      , metadata = Metadata.default
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
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
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
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
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Ints" ] [ Type.Int ]
                                      , implementation =
                                            AST.SoloImpl
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                      }
                                    , { name = "main"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.asEntryPoint
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
                                        [ Type.Custom "Dollar"
                                        , Type.Custom "Cent"
                                        ]
                                    , AST.CustomTypeDef
                                        emptyRange
                                        "Dollar"
                                        []
                                        [ ( "dollar-value", Type.Int ) ]
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
                                        [ ( "box", Type.Custom "Box" ) ]
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
