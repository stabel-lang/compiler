module Play.Qualifier.Problem exposing (Problem(..))

import Play.Data.SourceLocation exposing (SourceLocationRange)


type Problem
    = UnknownWordRef SourceLocationRange String
    | UnknownTypeRef SourceLocationRange String
    | UnionTypeMatchWithPatterns SourceLocationRange
    | InvalidTypeMatch SourceLocationRange
    | NoSuchMemberOnType String String
