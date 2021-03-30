module Play.Data.SemanticVersion exposing
    ( Compatibility(..)
    , ParseError(..)
    , SemanticVersion
    , compatible
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
            fromInts str major minor patch

        _ ->
            Err <| InvalidFormat str


fromInts : String -> String -> String -> String -> Result ParseError SemanticVersion
fromInts originalStr majorStr minorStr patchStr =
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


type Compatibility
    = Incompatible
    | LessThan
    | GreaterThanOrEqual


compatible : SemanticVersion -> SemanticVersion -> Compatibility
compatible (SemanticVersion lhsMajor lhsMinor lhsPatch) (SemanticVersion rhsMajor rhsMinor rhsPatch) =
    if lhsMajor /= rhsMajor then
        Incompatible

    else
        case compare lhsMinor rhsMinor of
            GT ->
                if lhsMajor == 0 then
                    Incompatible

                else
                    LessThan

            LT ->
                if lhsMajor == 0 then
                    Incompatible

                else
                    GreaterThanOrEqual

            EQ ->
                case compare lhsPatch rhsPatch of
                    GT ->
                        LessThan

                    EQ ->
                        GreaterThanOrEqual

                    LT ->
                        GreaterThanOrEqual
