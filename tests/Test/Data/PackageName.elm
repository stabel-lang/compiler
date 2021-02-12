module Test.Data.PackageName exposing (suite)

import Expect
import Play.Data.PackageName as PackageName
import Test exposing (Test, describe, test)
import Test.PlayExpect as PlayExpect


suite : Test
suite =
    describe "PackageName"
        [ test "valid package names" <|
            \_ ->
                PlayExpect.allOk PackageName.fromString
                    [ "some/package"
                    , "play/std_lib"
                    , "number1/package123"
                    , "play_std/lib"
                    ]
        , test "Must contain exactly two parts, seperated by a single slash" <|
            \_ ->
                PlayExpect.allErr PackageName.fromString
                    [ "/"
                    , "one/"
                    , "/one"
                    , "one"
                    , "one/two/three"
                    ]
        , test "Both parts of a name must start with lower cased ascii character" <|
            \_ ->
                PlayExpect.allErr PackageName.fromString
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
                PlayExpect.allErr PackageName.fromString
                    [ "myPack/name"
                    , "mypackage/someName"
                    , "mypackage/some_Name"
                    ]
        , test "Both parts of a name cannot contain non ascii or digit letters" <|
            \_ ->
                PlayExpect.allErr PackageName.fromString
                    [ "my#pack/name"
                    , "mypackage/bills$"
                    ]
        ]
