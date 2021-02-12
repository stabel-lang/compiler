module Test.Data.SemanticVersion exposing (suite)

import Expect
import Play.Data.SemanticVersion as SemanticVersion
import Test exposing (Test, describe, test)
import Test.PlayExpect as PlayExpect


suite : Test
suite =
    describe "SemanticVersion"
        [ test "Valid versions" <|
            \_ ->
                PlayExpect.allOk SemanticVersion.fromString
                    [ "1"
                    , "1.2"
                    , "1.2.3"
                    , "10.100.1000"
                    , "0.0.1"
                    , "0.1.0"
                    ]
        , test "Cannot contain more than three parts" <|
            \_ ->
                SemanticVersion.fromString "1.2.3.4"
                    |> Expect.err
        , test "Cannot be empty string" <|
            \_ ->
                SemanticVersion.fromString ""
                    |> Expect.err
        , test "Must be numbers" <|
            \_ ->
                PlayExpect.allErr SemanticVersion.fromString
                    [ "A"
                    , "*"
                    , "~"
                    , "1.0.x"
                    , "~1.0.0"
                    , "1.0.0-alpha1"
                    , "alpha.1"
                    ]
        , test "Minimum version is 0.0.1" <|
            \_ ->
                SemanticVersion.fromString "0.0.0"
                    |> Expect.err
        , test "Cannot contain negative versions" <|
            \_ ->
                PlayExpect.allErr SemanticVersion.fromString
                    [ "-1"
                    , "-1.2"
                    , "1.-2.0"
                    ]
        ]
