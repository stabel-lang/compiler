module Play.Data.SemanticVersion exposing
    ( SemanticVersion
    , fromString
    )


type SemanticVersion
    = SemanticVersion Int Int Int


fromString : String -> Result () SemanticVersion
fromString str =
    case String.split "." str of
        [ major ] ->
            toInt major Nothing Nothing

        [ major, minor ] ->
            toInt major (Just minor) Nothing

        [ major, minor, patch ] ->
            toInt major (Just minor) (Just patch)

        _ ->
            Err ()


toInt : String -> Maybe String -> Maybe String -> Result () SemanticVersion
toInt majorStr maybeMinorStr maybePatchStr =
    let
        minorStr =
            Maybe.withDefault "0" maybeMinorStr

        patchStr =
            Maybe.withDefault "0" maybePatchStr

        intVersions =
            [ majorStr, minorStr, patchStr ]
                |> List.filterMap String.toInt
    in
    case intVersions of
        [ major, minor, patch ] ->
            if major < 0 || minor < 0 || patch < 0 then
                Err ()

            else if major == 0 && minor == 0 && patch < 1 then
                Err ()

            else
                Ok <| SemanticVersion major minor patch

        _ ->
            Err ()
