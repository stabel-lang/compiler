module Play.Qualifier.Problem exposing
    ( Problem(..)
    , toString
    )

import Play.Data.SourceLocation exposing (SourceLocationRange)


type Problem
    = UnknownWordRef SourceLocationRange String
    | UnknownTypeRef SourceLocationRange String
    | UnionTypeMatchWithPatterns SourceLocationRange
    | InvalidTypeMatch SourceLocationRange
    | NoSuchMemberOnType String String


toString : Problem -> String
toString problem =
    case problem of
        _ ->
            "UNKNOWN"
