module Play.Parser.Problem exposing
    ( Problem(..)
    , toString
    )

import Play.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)


type Problem
    = NotInt
    | NotSymbol
    | NotMetadata
    | NotGeneric
    | NotType
    | NoProblem
    | FoundMetadata
    | ExpectedLeftParen
    | ExpectedRightParen
    | ExpectedEnd
    | ExpectedTypeSeperator
    | ExpectedLeftBracket
    | ExpectedRightBracket
    | WordAlreadyDefined String (Maybe SourceLocationRange) (Maybe SourceLocationRange)
    | TypeAlreadyDefined String SourceLocationRange SourceLocationRange
    | UnknownMetadata String
    | InvalidModulePath String


toString : String -> Problem -> String
toString source problem =
    case problem of
        NotInt ->
            "this is not an integer"

        NotSymbol ->
            "this is not a symbol"

        NotMetadata ->
            "this is not metadata"

        NotGeneric ->
            "this is not a generic variable"

        NotType ->
            "this is not a type"

        NoProblem ->
            "not sure how we got this error"

        FoundMetadata ->
            "found metadata where we did not expect too"

        ExpectedLeftParen ->
            "expected an opening parenthesis"

        ExpectedRightParen ->
            "expected an closing parenthesis"

        ExpectedEnd ->
            "expected end of file"

        ExpectedTypeSeperator ->
            "expected type seperator"

        ExpectedLeftBracket ->
            "expected opening bracket"

        ExpectedRightBracket ->
            "expected closing brakcet"

        WordAlreadyDefined wordName maybePreviousDefinitionRange maybeDefinitionRange ->
            let
                definitionRange =
                    Maybe.withDefault SourceLocation.emptyRange maybeDefinitionRange
            in
            case maybePreviousDefinitionRange of
                Nothing ->
                    SourceLocation.toString definitionRange.start
                        ++ ": You're trying to define a new word called '"
                        ++ wordName
                        ++ "', but this word has already been defined."

                Just previousDefinitionRange ->
                    SourceLocation.toString definitionRange.start
                        ++ ": You're trying to define a new word called '"
                        ++ wordName
                        ++ "', but this word has already been defined here:\n\n"
                        ++ SourceLocation.extractFromString source previousDefinitionRange

        TypeAlreadyDefined typeName previousDefinitionRange definitionRange ->
            SourceLocation.toString definitionRange.start
                ++ ": You're trying to define a new type called '"
                ++ typeName
                ++ "', but this type has already been defined here:\n\n"
                ++ SourceLocation.extractFromString source previousDefinitionRange

        UnknownMetadata meta ->
            meta ++ " is not a known metadata label."

        InvalidModulePath path ->
            "'" ++ path ++ "' is not a valid module path. Note: Upper case characters are not allowed."
