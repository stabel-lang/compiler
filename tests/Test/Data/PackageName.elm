module Test.Data.PackageName exposing (suite)

import Expect
import Play.Data.PackageName as PackageName
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PackageName"
        [ test "valid package names" <|
            \_ ->
                expectAllOk PackageName.fromString
                    [ "some/package"
                    , "play/std_lib"
                    , "number1/package123"
                    , "play_std/lib"
                    ]
        , test "Must contain exactly two parts, seperated by a single slash" <|
            \_ ->
                expectAllErr PackageName.fromString
                    [ "/"
                    , "one/"
                    , "/one"
                    , "one"
                    , "one/two/three"
                    ]
        , test "Both parts of a name must start with lower cased ascii character" <|
            \_ ->
                expectAllErr PackageName.fromString
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
                expectAllErr PackageName.fromString
                    [ "myPack/name"
                    , "mypackage/someName"
                    , "mypackage/some_Name"
                    ]
        , test "Both parts of a name cannot contain non ascii or digit letters" <|
            \_ ->
                expectAllErr PackageName.fromString
                    [ "my#pack/name"
                    , "mypackage/bills$"
                    ]
        ]


expectAllOk : (a -> Result err ok) -> List a -> Expect.Expectation
expectAllOk fn values =
    let
        expectationList =
            List.map (\val -> \f -> f val |> expectOk val) values

        expectOk original result =
            case result of
                Ok _ ->
                    Expect.pass

                Err _ ->
                    Expect.fail <| "Expected Ok for input " ++ Debug.toString original ++ ", was: " ++ Debug.toString result
    in
    Expect.all expectationList fn


expectAllErr : (a -> Result err ok) -> List a -> Expect.Expectation
expectAllErr fn values =
    let
        expectationList =
            List.map (\val -> \f -> f val |> expectErr val) values

        expectErr original result =
            case result of
                Ok _ ->
                    Expect.fail <| "Expected Err for input " ++ Debug.toString original ++ ", was: " ++ Debug.toString result

                Err _ ->
                    Expect.pass
    in
    Expect.all expectationList fn
