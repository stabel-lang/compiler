module Stabel.Qualifier.Problem exposing
    ( Problem(..)
    , sourceLocationRef
    , toString
    )

import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)


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
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "No such function: '"
                ++ functionRef
                ++ "'"

        UnknownTypeRef range typeRef ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "No such type: '"
                ++ typeRef
                ++ "'"

        UnionTypeMatchWithPatterns range ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Union types cannot have sub-patterns."

        InvalidTypeMatch range ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "This is not a valid pattern match. Pattern matches look like Type( <member> <value> )."

        NoSuchMemberOnType range typeName member ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ typeName
                ++ " does not have a member called '"
                ++ member
                ++ "'"

        FunctionNotExposed range functionRef ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Trying to call '"
                ++ functionRef
                ++ "' but this function is not exposed."

        TypeNotExposed range typeRef ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Referencing '"
                ++ typeRef
                ++ "' but this type is not exposed."


sourceLocationRef : Problem -> String
sourceLocationRef problem =
    case problem of
        UnknownFunctionRef range _ ->
            range.source

        UnknownTypeRef range _ ->
            range.source

        UnionTypeMatchWithPatterns range ->
            range.source

        InvalidTypeMatch range ->
            range.source

        NoSuchMemberOnType range _ _ ->
            range.source

        FunctionNotExposed range _ ->
            range.source

        TypeNotExposed range _ ->
            range.source
