module Stabel.Qualifier.Problem exposing
    ( Problem(..)
    , toString
    )

import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)


type Problem
    = UnknownWordRef SourceLocationRange String
    | UnknownTypeRef SourceLocationRange String
    | UnionTypeMatchWithPatterns SourceLocationRange
    | InvalidTypeMatch SourceLocationRange
    | NoSuchMemberOnType SourceLocationRange String String
    | WordNotExposed SourceLocationRange String
    | TypeNotExposed SourceLocationRange String


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

        WordNotExposed range wordRef ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "Trying to call '"
                ++ wordRef
                ++ "' but this function is not exposed."

        TypeNotExposed range typeRef ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "Referencing '"
                ++ typeRef
                ++ "' but this type is not exposed."
