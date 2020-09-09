module Test.Qualifier.Error exposing (..)

import Expect exposing (Expectation)
import Play.Parser as Parser
import Play.Qualifier exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier errors"
        []


checkForError : (Problem -> Bool) -> Parser.AST -> Expectation
checkForError fn source =
    case run source of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect parsing to succeed"
