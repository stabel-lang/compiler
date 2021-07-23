module Test.TypeChecker.Util exposing
    ( checkForError
    , expectAst
    , expectTypeCheck
    , expectTypeCheckFailure
    , getTypeErrors
    )

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Stabel.Parser as Parser
import Stabel.Qualifier as Qualifier
import Stabel.TypeChecker as TypeChecker exposing (FunctionDefinition)
import Stabel.TypeChecker.Problem exposing (Problem)
import Test.Qualifier.Util as QualifierUtil


expectTypeCheck : String -> Expectation
expectTypeCheck input =
    case qualifyInput input of
        Err err ->
            Expect.fail err

        Ok qualifiedAst ->
            case TypeChecker.run qualifiedAst of
                Err err ->
                    Expect.fail <| "Did not expect typecheck to fail: " ++ Debug.toString err

                Ok _ ->
                    Expect.pass


expectTypeCheckFailure : String -> Expectation
expectTypeCheckFailure input =
    case qualifyInput input of
        Err err ->
            Expect.fail err

        Ok qualifiedAst ->
            case TypeChecker.run qualifiedAst of
                Err _ ->
                    Expect.pass

                Ok _ ->
                    Expect.fail "Did not expect type check to succeed."


expectAst : String -> Dict String FunctionDefinition -> Expectation
expectAst input expectedResult =
    case qualifyInput input of
        Err err ->
            Expect.fail err

        Ok qualifiedAst ->
            case TypeChecker.run qualifiedAst of
                Err err ->
                    Expect.fail <| "Did not expect typecheck to fail: " ++ Debug.toString err

                Ok typedAst ->
                    Expect.equal expectedResult typedAst.functions


checkForError : (Problem -> Bool) -> String -> Expectation
checkForError fn input =
    case qualifyInput input of
        Err err ->
            Expect.fail err

        Ok qualifiedAst ->
            case TypeChecker.run qualifiedAst of
                Err errors ->
                    if List.any fn errors then
                        Expect.pass

                    else
                        Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

                Ok _ ->
                    Expect.fail "Expected error."


getTypeErrors : String -> List Problem
getTypeErrors input =
    case qualifyInput input of
        Err _ ->
            []

        Ok qualifiedAst ->
            case TypeChecker.run qualifiedAst of
                Err errors ->
                    errors

                Ok _ ->
                    []


qualifyInput : String -> Result String Qualifier.AST
qualifyInput source =
    case Parser.run "test" source of
        Err err ->
            Err <| "Parse error: " ++ Debug.toString err

        Ok parserAst ->
            let
                qualifierResult =
                    Qualifier.run
                        { packageName = ""
                        , modulePath = ""
                        , ast = parserAst
                        , externalModules = Dict.empty
                        , inProgressAST = QualifierUtil.emptyAst
                        }
            in
            case qualifierResult of
                Err err ->
                    Err <| "Qualifier error: " ++ Debug.toString err

                Ok qualifiedAst ->
                    Ok <| QualifierUtil.stripLocations qualifiedAst
