module Stabel.Parser.Problem exposing
    ( Context(..)
    , Problem(..)
    , toString
    )

import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)


type Context
    = TopLevel
    | ModuleDefinition
    | AliasKeyword
    | ImportKeyword
    | ExposingKeyword
    | FunctionDefinition String
    | MultifunctionDefinition String
    | StructDefinition String
    | UnionDefinition String
    | TypeKeyword
    | MemberKeyword
    | ImplementationKeyword
    | ElseKeyword


type Problem
    = ExpectedInt
    | ExpectedSymbol
    | ExpectedMetadata
    | ExpectedGeneric
    | ExpectedType
    | UnknownError
    | ExpectedForwardSlash
    | ExpectedWhitespace
    | UnexpectedMetadata
    | ExpectedLeftParen
    | ExpectedRightParen
    | ExpectedEndOfFile
    | ExpectedTypeSeperator
    | ExpectedLeftBracket
    | ExpectedRightBracket
    | FunctionAlreadyDefined String (Maybe SourceLocationRange) (Maybe SourceLocationRange)
    | TypeAlreadyDefined String SourceLocationRange SourceLocationRange
    | UnknownMetadata String
    | InvalidModulePath String
    | ModuleIsEmpty
    | BadDefinition SourceLocationRange String


toString : String -> Problem -> String
toString source problem =
    case problem of
        ExpectedInt ->
            "this is not an integer"

        ExpectedSymbol ->
            "this is not a symbol"

        ExpectedMetadata ->
            "this is not metadata"

        ExpectedGeneric ->
            "this is not a generic variable"

        ExpectedType ->
            "this is not a type"

        UnknownError ->
            "not sure how we got this error"

        ExpectedForwardSlash ->
            "Expected a forward slash"

        ExpectedWhitespace ->
            "Expected whitespace"

        UnexpectedMetadata ->
            "found metadata where we did not expect too"

        ExpectedLeftParen ->
            "expected an opening parenthesis"

        ExpectedRightParen ->
            "expected an closing parenthesis"

        ExpectedEndOfFile ->
            "expected end of file"

        ExpectedTypeSeperator ->
            "expected type seperator"

        ExpectedLeftBracket ->
            "expected opening bracket"

        ExpectedRightBracket ->
            "expected closing brakcet"

        FunctionAlreadyDefined functionName maybePreviousDefinitionRange maybeDefinitionRange ->
            let
                definitionRange =
                    Maybe.withDefault SourceLocation.emptyRange maybeDefinitionRange
            in
            case maybePreviousDefinitionRange of
                Nothing ->
                    SourceLocation.toString definitionRange.start
                        ++ ": You're trying to define a new function called '"
                        ++ functionName
                        ++ "', but this function has already been defined."

                Just previousDefinitionRange ->
                    SourceLocation.toString definitionRange.start
                        ++ ": You're trying to define a new function called '"
                        ++ functionName
                        ++ "', but this function has already been defined here:\n\n"
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

        ModuleIsEmpty ->
            "A module is required to contain at least one definition."

        BadDefinition sourceRange name ->
            SourceLocation.extractFromString source sourceRange
                ++ "\n\n"
                ++ "'"
                ++ name
                ++ "' is not a valid definition. Expected either def:, defmulti:, defstruct: or defunion:"
