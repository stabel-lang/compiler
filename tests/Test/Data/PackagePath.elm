module Test.Data.PackagePath exposing (suite)

import Expect
import Stabel.Data.PackagePath as PackagePath
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PackagePath"
        [ test "Any path not ending in * is a directory" <|
            \_ ->
                Expect.equal
                    (PackagePath.Directory "/some/path")
                    (PackagePath.fromString "/some/path")
        , test "Any path ending in * is a recursive directory path" <|
            \_ ->
                Expect.equal
                    (PackagePath.AllDirectoriesInDirectory "/some/path")
                    (PackagePath.fromString "/some/path/*")
        , test "Final slash is removed" <|
            \_ ->
                Expect.equal
                    (PackagePath.Directory "/some/path")
                    (PackagePath.fromString "/some/path/")
        ]
