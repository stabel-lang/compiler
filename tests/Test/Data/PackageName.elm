module Test.Data.PackageName exposing (suite)

import Stabel.Data.PackageName as PackageName
import Test exposing (Test, describe, test)
import Test.StabelExpect as StabelExpect


suite : Test
suite =
    describe "PackageName"
        [ test "valid package names" <|
            \_ ->
                StabelExpect.allOk PackageName.fromString
                    [ "some/package"
                    , "stabel/std_lib"
                    , "number1/package123"
                    , "stabel_std/lib"
                    ]
        , test "Must contain exactly two parts, seperated by a single slash" <|
            \_ ->
                StabelExpect.allErr PackageName.fromString
                    [ "/"
                    , "one/"
                    , "/one"
                    , "one"
                    , "one/two/three"
                    ]
        , test "Both parts of a name must start with lower cased ascii character" <|
            \_ ->
                StabelExpect.allErr PackageName.fromString
                    [ "1pack/name"
                    , "_priv/pack"
                    , "#whaaat/events"
                    , "Some/package"
                    , "pack/1name"
                    , "priv/_pack"
                    , "whaaat/#events"
                    , "some/Package"
                    ]
        , test "Both parts of a name cannot contain upper case letters" <|
            \_ ->
                StabelExpect.allErr PackageName.fromString
                    [ "myPack/name"
                    , "mypackage/someName"
                    , "mypackage/some_Name"
                    ]
        , test "Both parts of a name cannot contain non ascii or digit letters" <|
            \_ ->
                StabelExpect.allErr PackageName.fromString
                    [ "my#pack/name"
                    , "mypackage/bills$"
                    ]
        ]
