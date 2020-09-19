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
                            { types = Dict.empty
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

                        noSuchReferenceError name problem =
                            case problem of
                                UnknownWordRef _ problemName ->
                                    name == problemName

                                _ ->
                                    False
                    in
                    checkForError (noSuchReferenceError "dec") ast
            , test "Type" <|
                \_ ->
                    let
                        ast =
                            { types = Dict.empty
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

                        noSuchReferenceError name problem =
                            case problem of
                                UnknownTypeRef _ problemName ->
                                    name == problemName

                                _ ->
                                    False
                    in
                    checkForError (noSuchReferenceError "Ints") ast
            ]
        ]


checkForError : (Problem -> Bool) -> AST.AST -> Expectation
checkForError fn source =
    case run source of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect parsing to succeed"
