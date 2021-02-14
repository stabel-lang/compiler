module Play.Data.SemanticVersion exposing
    ( SemanticVersion
    , fromString
    )


type SemanticVersion
    = SemanticVersion Int Int Int


fromString : String -> Result () SemanticVersion
fromString str =
    case String.split "." str of
        [ major, minor, patch ] ->
            toInt major minor patch

        _ ->
            Err ()


toInt : String -> String -> String -> Result () SemanticVersion
toInt majorStr minorStr patchStr =
    let
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
