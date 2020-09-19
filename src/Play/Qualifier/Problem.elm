module Play.Qualifier.Problem exposing
    ( Problem(..)
    , toString
    )

import Play.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)


type Problem
    = UnknownWordRef SourceLocationRange String
    | UnknownTypeRef SourceLocationRange String
    | UnionTypeMatchWithPatterns SourceLocationRange
    | InvalidTypeMatch SourceLocationRange
    | NoSuchMemberOnType SourceLocationRange String String


toString : String -> Problem -> String
toString source problem =
    case problem of
        UnknownWordRef range wordRef ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "No such word: '"
                ++ wordRef
                ++ "'."

        UnknownTypeRef range typeRef ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "No such type: '"
                ++ typeRef
                ++ "'."

        UnionTypeMatchWithPatterns range ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "Union types cannot have sub-patterns."

        InvalidTypeMatch range ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "This is not a valid pattern match. Pattern matches look like Type( <member> <value> )."

        NoSuchMemberOnType range typeName member ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ typeName
                ++ " does not have a member called '"
                ++ member
                ++ "'."
