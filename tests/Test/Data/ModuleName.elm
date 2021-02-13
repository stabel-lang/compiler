module Test.Data.ModuleName exposing (suite)

import Expect
import Fuzz
import Play.Data.ModuleName as ModuleName
import Play.Data.PackageName as PackageName
import Test exposing (Test, describe, fuzz, test)
import Test.PlayExpect as PlayExpect


suite : Test
suite =
    describe "ModuleName"
        [ fuzz Fuzz.string "Valid 2-part module names are also valid package names" <|
            \name ->
                let
                    combinedModuleName =
                        String.join "/" [ name, name ]
                in
                expectSameResult
                    (PackageName.fromString combinedModuleName)
                    (ModuleName.fromString combinedModuleName)
        , test "Module names can have only one part" <|
            \_ ->
                Expect.ok <| ModuleName.fromString "dict"
        , test "Module names can have many parts" <|
            \_ ->
                Expect.ok <| ModuleName.fromString "stdlib/dict/ordered"
        , test "The empty string is not a valid module name" <|
            \_ ->
                Expect.err <| ModuleName.fromString ""
        , test "Module names cannot start with a slash" <|
            \_ ->
                Expect.err <| ModuleName.fromString "/dict"
        , test "Module names cannot end with a slash" <|
            \_ ->
                Expect.err <| ModuleName.fromString "dict/"
        ]


expectSameResult : Result aErr aOk -> Result bErr bOk -> Expect.Expectation
expectSameResult left right =
    case ( left, right ) of
        ( Ok _, Ok _ ) ->
            Expect.pass

        ( Err _, Err _ ) ->
            Expect.pass

        _ ->
            Expect.fail <| "Different results: " ++ Debug.toString ( left, right )
