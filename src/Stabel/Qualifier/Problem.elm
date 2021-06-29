module Stabel.Qualifier.Problem exposing
    ( Problem(..)
    , toString
    )

import Stabel.Qualifier.SourceLocation as SourceLocation exposing (SourceLocationRange)


type Problem
    = UnknownFunctionRef SourceLocationRange String
    | UnknownTypeRef SourceLocationRange String
    | UnionTypeMatchWithPatterns SourceLocationRange
    | InvalidTypeMatch SourceLocationRange
    | NoSuchMemberOnType SourceLocationRange String String
    | FunctionNotExposed SourceLocationRange String
    | TypeNotExposed SourceLocationRange String


toString : String -> Problem -> String
toString source problem =
    case problem of
        UnknownFunctionRef range functionRef ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "No such function: '"
                ++ functionRef
                ++ "'."

        UnknownTypeRef range typeRef ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "No such type: '"
                ++ typeRef
                ++ "'."

        UnionTypeMatchWithPatterns range ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Union types cannot have sub-patterns."

        InvalidTypeMatch range ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "This is not a valid pattern match. Pattern matches look like Type( <member> <value> )."

        NoSuchMemberOnType range typeName member ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ typeName
                ++ " does not have a member called '"
                ++ member
                ++ "'."

        FunctionNotExposed range functionRef ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Trying to call '"
                ++ functionRef
                ++ "' but this function is not exposed."

        TypeNotExposed range typeRef ->
            SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Referencing '"
                ++ typeRef
                ++ "' but this type is not exposed."
