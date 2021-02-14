module Test.PackageLoader exposing (suite)

import Expect
import Fuzz
import Play.PackageLoader as PackageLoader
import Test exposing (Test, describe, fuzz, test)
import Test.PlayExpect as PlayExpect


suite : Test
suite =
    describe "PackageLoader"
        [ test "Passes the load package metadata step" <|
            \_ ->
                PackageLoader.init "play.json"
                    """
                    {
                        "name": "robheghan/fnv",
                        "version": "1.0.0",
                        "language-version": "0.2.0",
                        "exposed-modules": [
                            "fnv"
                        ],
                        "dependencies": {
                            "template_string": "1.2.0"
                        },
                        "package-paths": [
                            "lib/*"
                        ]
                    }
                    """
                    |> Expect.ok
        ]
