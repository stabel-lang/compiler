module Play.Parser.Problem exposing
    ( Problem(..)
    , toString
    )

import Play.Data.SourceLocation exposing (SourceLocationRange)


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


toString : Problem -> String
toString problem =
    case problem of
        _ ->
            "UNKNOWN"
