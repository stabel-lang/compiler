module Test.TypeChecker.Util exposing
    ( expectAst
    , expectTypeCheck
    , expectTypeCheckFailure
    )

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Stabel.Parser as Parser
import Stabel.Qualifier as Qualifier
import Stabel.TypeChecker as TypeChecker exposing (FunctionDefinition)
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
                    Ok qualifiedAst
