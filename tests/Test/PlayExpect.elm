module Test.PlayExpect exposing
    ( allEqual
    , allErr
    , allOk
    )

import Expect


allOk : (a -> Result err ok) -> List a -> Expect.Expectation
allOk fn values =
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


allErr : (a -> Result err ok) -> List a -> Expect.Expectation
allErr fn values =
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


allEqual : (a -> b) -> List ( a, b ) -> Expect.Expectation
allEqual fn values =
    let
        expectationList =
            List.map (\( input, expected ) -> \f -> Expect.equal expected (f input)) values
    in
    Expect.all expectationList fn
