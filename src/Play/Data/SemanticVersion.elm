module Play.Data.SemanticVersion exposing
    ( ParseError(..)
    , SemanticVersion
    , fromString
    )


type SemanticVersion
    = SemanticVersion Int Int Int


type ParseError
    = InvalidFormat String
    | ExpectedInteger String
    | NegativeVersions String
    | LessThanMinimumVersion String


fromString : String -> Result ParseError SemanticVersion
fromString str =
    case String.split "." str of
        [ major, minor, patch ] ->
            toInt str major minor patch

        _ ->
            Err <| InvalidFormat str


toInt : String -> String -> String -> String -> Result ParseError SemanticVersion
toInt originalStr majorStr minorStr patchStr =
    let
        intVersions =
            [ majorStr, minorStr, patchStr ]
                |> List.filterMap String.toInt
    in
    case intVersions of
        [ major, minor, patch ] ->
            if major < 0 || minor < 0 || patch < 0 then
                Err <| NegativeVersions originalStr

            else if major == 0 && minor == 0 && patch < 1 then
                Err <| LessThanMinimumVersion originalStr

            else
                Ok <| SemanticVersion major minor patch

        _ ->
            Err <| ExpectedInteger originalStr
