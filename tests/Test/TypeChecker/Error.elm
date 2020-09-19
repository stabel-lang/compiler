module Test.TypeChecker.Error exposing (..)

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type
import Play.Qualifier exposing (..)
import Play.TypeChecker as TypeChecker
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TypeChecker errors"
        [ test "Undeclared generic" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ CustomTypeDef "Box"
                                    emptyRange
                                    [ "a" ]
                                    [ ( "value", Type.Generic "b" ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 2
                                            ]
                                  }
                                ]
                        }

                    undeclaredGenericError generic problem =
                        case problem of
                            TypeChecker.UndeclaredGeneric _ problemGeneric _ ->
                                generic == problemGeneric

                            _ ->
                                False
                in
                checkForError (undeclaredGenericError "b") ast
        , test "Wrong type signature" <|
            \_ ->
                let
                    ast =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Integer emptyRange 2
                                            ]
                                  }
                                ]
                        }

                    undeclaredGenericError problem =
                        case problem of
                            TypeChecker.TypeError _ "main" provided inferred ->
                                (provided == { input = [], output = [ Type.Int ] })
                                    && (inferred == { input = [], output = [ Type.Int, Type.Int ] })

                            _ ->
                                False
                in
                checkForError undeclaredGenericError ast
        , test "Unexpected function" <|
            \_ ->
                let
                    ast =
                        { types =
                            Dict.fromListBy typeDefinitionName
                                [ CustomTypeDef "IntBox"
                                    emptyRange
                                    []
                                    [ ( "value", Type.Int ) ]
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                            |> Metadata.withType [] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ Integer emptyRange 1
                                            , Word emptyRange "value>"
                                            ]
                                  }
                                , { name = ">IntBox"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Int ] [ Type.Custom "IntBox" ]
                                  , implementation =
                                        SoloImpl
                                            [ ConstructType "IntBox" ]
                                  }
                                , { name = ">value"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "IntBox", Type.Int ] [ Type.Custom "IntBox" ]
                                  , implementation =
                                        SoloImpl
                                            [ SetMember "IntBox" "value" ]
                                  }
                                , { name = "value>"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [ Type.Custom "IntBox" ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ GetMember "IntBox" "value" ]
                                  }
                                ]
                        }

                    undeclaredGenericError problem =
                        case problem of
                            TypeChecker.UnexpectedType _ "main" (Type.Custom "IntBox") Type.Int ->
                                True

                            _ ->
                                False
                in
                checkForError undeclaredGenericError ast
        ]


checkForError : (TypeChecker.Problem -> Bool) -> AST -> Expectation
checkForError fn source =
    case TypeChecker.run source of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect parsing to succeed"
