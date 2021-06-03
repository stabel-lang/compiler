module Test.Data.SemanticVersion exposing (suite)

import Expect
import Stabel.Data.SemanticVersion as SemanticVersion
import Test exposing (Test, describe, test)
import Test.StabelExpect as StabelExpect


suite : Test
suite =
    describe "SemanticVersion"
        [ test "Valid versions" <|
            \_ ->
                StabelExpect.allOk SemanticVersion.fromString
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
                StabelExpect.allEqual SemanticVersion.fromString
                    [ ( "1", Err <| SemanticVersion.InvalidFormat "1" )
                    , ( "1.2", Err <| SemanticVersion.InvalidFormat "1.2" )
                    ]
        , test "Cannot be empty string" <|
            \_ ->
                SemanticVersion.fromString ""
                    |> Expect.equal (Err <| SemanticVersion.InvalidFormat "")
        , test "Must be numbers" <|
            \_ ->
                StabelExpect.allEqual SemanticVersion.fromString
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
                StabelExpect.allEqual SemanticVersion.fromString
                    [ ( "-1.0.0", Err <| SemanticVersion.NegativeVersions "-1.0.0" )
                    , ( "1.-2.0", Err <| SemanticVersion.NegativeVersions "1.-2.0" )
                    , ( "1.2.-3", Err <| SemanticVersion.NegativeVersions "1.2.-3" )
                    ]
        , describe "Compatibility" <|
            let
                helper ( lhs, rhs ) =
                    Result.map2 SemanticVersion.compatible (SemanticVersion.fromString lhs) (SemanticVersion.fromString rhs)
            in
            [ test "Same version, or greater version, returns GreaterThanOrEqual" <|
                \_ ->
                    StabelExpect.allEqual helper
                        [ ( ( "1.0.0", "1.0.0" ), Ok SemanticVersion.GreaterThanOrEqual )
                        , ( ( "0.2.1", "0.2.1" ), Ok SemanticVersion.GreaterThanOrEqual )
                        , ( ( "2.0.8", "2.0.8" ), Ok SemanticVersion.GreaterThanOrEqual )
                        , ( ( "1.0.0", "1.2.0" ), Ok SemanticVersion.GreaterThanOrEqual )
                        , ( ( "2.1.0", "2.1.1" ), Ok SemanticVersion.GreaterThanOrEqual )
                        , ( ( "1.2.3", "1.3.0" ), Ok SemanticVersion.GreaterThanOrEqual )
                        ]
            , test "Lesser version returns LessThan" <|
                \_ ->
                    StabelExpect.allEqual helper
                        [ ( ( "0.2.1", "0.2.0" ), Ok SemanticVersion.LessThan )
                        , ( ( "1.1.0", "1.0.5" ), Ok SemanticVersion.LessThan )
                        , ( ( "2.1.3", "2.1.1" ), Ok SemanticVersion.LessThan )
                        , ( ( "1.2.3", "1.2.2" ), Ok SemanticVersion.LessThan )
                        ]
            , test "Incompatible versions" <|
                \_ ->
                    StabelExpect.allEqual helper
                        [ ( ( "0.2.1", "0.3.0" ), Ok SemanticVersion.Incompatible )
                        , ( ( "1.0.0", "2.0.0" ), Ok SemanticVersion.Incompatible )
                        , ( ( "1.2.0", "2.2.0" ), Ok SemanticVersion.Incompatible )
                        , ( ( "0.0.1", "1.0.1" ), Ok SemanticVersion.Incompatible )
                        ]
            ]
        ]
