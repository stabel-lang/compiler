module Test.EntryPoints exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "EntryPoints"
        [ test "Dummy test to see that code not covered by test at least compiles" <|
            \_ ->
                Expect.pass
        ]
