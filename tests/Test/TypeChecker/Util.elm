module Test.TypeChecker.Util exposing
    ( expectAst
    , expectTypeCheck
    , expectTypeCheckFailure
    )

import Expect exposing (Expectation)
import Stabel.Qualifier as QAST
import Stabel.TypeChecker exposing (AST, run)


expectTypeCheck : QAST.AST -> Expectation
expectTypeCheck input =
    case run input of
        Err err ->
            Expect.fail <| "Did not expect typecheck to fail: " ++ Debug.toString err

        Ok _ ->
            Expect.pass


expectTypeCheckFailure : QAST.AST -> Expectation
expectTypeCheckFailure input =
    case run input of
        Err err ->
            Expect.pass

        Ok _ ->
            Expect.fail "Did not expect type check to succeed."


expectAst : QAST.AST -> AST -> Expectation
expectAst input expectedResult =
    case run input of
        Err err ->
            Expect.fail <| "Did not expect typecheck to fail: " ++ Debug.toString err

        Ok typedAst ->
            Expect.equal expectedResult typedAst
