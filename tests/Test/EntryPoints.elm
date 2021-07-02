module Test.EntryPoints exposing (suite)

import CLI
import Expect
import Test exposing (Test, describe, test)
import TestCompiler


suite : Test
suite =
    describe "EntryPoints"
        [ test "Dummy test to see that code not covered by test at least compiles" <|
            \_ ->
                Expect.pass
        ]
