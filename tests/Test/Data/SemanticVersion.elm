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
                    [ "1.2.3"
                    , "10.100.1000"
                    , "0.0.1"
                    , "0.1.0"
                    ]
        , test "Cannot contain more than three parts" <|
            \_ ->
                SemanticVersion.fromString "1.2.3.4"
                    |> Expect.equal (Err <| SemanticVersion.InvalidFormat "1.2.3.4")
        , test "Cannot contain less than three parts" <|
            \_ ->
                PlayExpect.allEqual SemanticVersion.fromString
                    [ ( "1", Err <| SemanticVersion.InvalidFormat "1" )
                    , ( "1.2", Err <| SemanticVersion.InvalidFormat "1.2" )
                    ]
        , test "Cannot be empty string" <|
            \_ ->
                SemanticVersion.fromString ""
                    |> Expect.equal (Err <| SemanticVersion.InvalidFormat "")
        , test "Must be numbers" <|
            \_ ->
                PlayExpect.allEqual SemanticVersion.fromString
                    [ ( "A", Err <| SemanticVersion.InvalidFormat "A" )
                    , ( "*", Err <| SemanticVersion.InvalidFormat "*" )
                    , ( "~", Err <| SemanticVersion.InvalidFormat "~" )
                    , ( "1.0.x", Err <| SemanticVersion.ExpectedInteger "1.0.x" )
                    , ( "1.0.~", Err <| SemanticVersion.ExpectedInteger "1.0.~" )
                    , ( "~1.0.0", Err <| SemanticVersion.ExpectedInteger "~1.0.0" )
                    , ( "1.0.0-alpha1", Err <| SemanticVersion.ExpectedInteger "1.0.0-alpha1" )
                    , ( "alpha.1", Err <| SemanticVersion.InvalidFormat "alpha.1" )
                    ]
        , test "Minimum version is 0.0.1" <|
            \_ ->
                SemanticVersion.fromString "0.0.0"
                    |> Expect.equal (Err <| SemanticVersion.LessThanMinimumVersion "0.0.0")
        , test "Cannot contain negative versions" <|
            \_ ->
                PlayExpect.allEqual SemanticVersion.fromString
                    [ ( "-1.0.0", Err <| SemanticVersion.NegativeVersions "-1.0.0" )
                    , ( "1.-2.0", Err <| SemanticVersion.NegativeVersions "1.-2.0" )
                    , ( "1.2.-3", Err <| SemanticVersion.NegativeVersions "1.2.-3" )
                    ]
        ]
