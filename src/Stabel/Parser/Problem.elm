module Stabel.Parser.Problem exposing
    ( Context(..)
    , Problem(..)
    , toString
    )

import Parser.Advanced exposing (DeadEnd)
import Stabel.Data.SourceLocation exposing (extractFromString)
import Stabel.Parser.SourceLocation
    exposing
        ( SourceLocation
        , SourceLocationRange
        )


type Context
    = ModuleDefinition
    | FunctionDefinition SourceLocation String
    | MultifunctionDefinition SourceLocation String
    | StructDefinition SourceLocation String
    | UnionDefinition SourceLocation String
    | AliasKeyword
    | ImportKeyword
    | ExposingKeyword
    | TypeKeyword
    | DocKeyword
    | MemberKeyword
    | ImplementationKeyword
    | ElseKeyword
    | IntegerLiteral
    | ArrayLiteral
    | StringLiteral


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
    | ExpectedLeftCurly
    | ExpectedRightCurly
    | FunctionAlreadyDefined String (Maybe SourceLocationRange)
    | TypeAlreadyDefined String SourceLocationRange
    | UnknownMetadata String
    | InvalidModulePath String
    | ModuleIsEmpty
    | BadDefinition String
    | UnknownEscapeSequence String
    | StringNotTerminated
    | IntegerBadLeadingZero
    | IntegerTrailingUnderscore
    | IntegerOutOfBounds


toString : String -> String -> DeadEnd Context Problem -> String
toString sourceRef source deadEnd =
    let
        contextExplination =
            contextStackExplination deadEnd.contextStack

        lineOfProblem =
            deadEnd.row

        lineOfContext =
            firstContextRow deadEnd
                |> Maybe.withDefault lineOfProblem

        ( startLine, endLine ) =
            ( min lineOfProblem lineOfContext
            , max lineOfProblem lineOfContext
            )

        startLoc =
            { row = startLine, col = 1 }

        endLoc =
            { row = endLine, col = 1000 }

        codeBlock =
            extractFromString source startLoc endLoc

        problemDetail =
            problemToString source deadEnd.problem
    in
    [ ">> " ++ sourceRef
    , contextExplination
    , codeBlock
    , problemDetail
    ]
        |> List.filter (not << String.isEmpty)
        |> String.join "\n\n"


contextStackExplination : List { row : Int, col : Int, context : Context } -> String
contextStackExplination contextStack =
    case contextStack of
        [] ->
            "I came across a problem"

        contextFrame :: [] ->
            "I came across a problem while parsing the "
                ++ contextToString contextFrame.context

        _ ->
            let
                contextStrings =
                    contextStack
                        |> List.map (.context >> contextToString)
                        |> String.join " of the "
            in
            "I came across a problem while parsing the "
                ++ contextStrings


contextToString : Context -> String
contextToString context =
    case context of
        ModuleDefinition ->
            "module definition"

        FunctionDefinition _ name ->
            "'" ++ name ++ "' function"

        MultifunctionDefinition _ name ->
            "'" ++ name ++ "' multi-function"

        StructDefinition _ name ->
            "'" ++ name ++ "' struct"

        UnionDefinition _ name ->
            "'" ++ name ++ "' union"

        AliasKeyword ->
            "alias keyword"

        ImportKeyword ->
            "import keyword"

        ExposingKeyword ->
            "exposing keyword"

        TypeKeyword ->
            "type keyword"

        DocKeyword ->
            "doc keyword"

        MemberKeyword ->
            "member"

        ImplementationKeyword ->
            "implementation"

        ElseKeyword ->
            "else branch"

        IntegerLiteral ->
            "integer"

        ArrayLiteral ->
            "array"

        StringLiteral ->
            "string"


firstContextRow : DeadEnd Context Problem -> Maybe Int
firstContextRow deadEnd =
    case deadEnd.contextStack of
        frame :: _ ->
            case frame.context of
                FunctionDefinition loc _ ->
                    Just loc.row

                MultifunctionDefinition loc _ ->
                    Just loc.row

                StructDefinition loc _ ->
                    Just loc.row

                UnionDefinition loc _ ->
                    Just loc.row

                _ ->
                    Just frame.row

        _ ->
            Nothing


problemToString : String -> Problem -> String
problemToString source problem =
    case problem of
        ExpectedInt ->
            "Expected to find an integer"

        ExpectedSymbol ->
            "Expected to find a symbol"

        ExpectedMetadata ->
            "Expected to find a keyword"

        ExpectedGeneric ->
            "Expected to find a generic variable"

        ExpectedType ->
            "Expected to find a type"

        UnknownError ->
            "Not sure why there is an error"

        ExpectedForwardSlash ->
            "Expected a forward slash"

        ExpectedWhitespace ->
            "Expected whitespace"

        UnexpectedMetadata ->
            "Found metadata where we did not expect too"

        ExpectedLeftParen ->
            "Expected a opening parenthesis"

        ExpectedRightParen ->
            "Expected a closing parenthesis"

        ExpectedEndOfFile ->
            "Expected end of file"

        ExpectedTypeSeperator ->
            "Expected type seperator (--)"

        ExpectedLeftBracket ->
            "Expected opening bracket"

        ExpectedRightBracket ->
            "Expected closing bracket"

        ExpectedLeftCurly ->
            "Expected opening curly brace"

        ExpectedRightCurly ->
            "Expected closing curly brace"

        FunctionAlreadyDefined functionName maybePreviousDefinitionRange ->
            case maybePreviousDefinitionRange of
                Nothing ->
                    "You're trying to define a new function called '"
                        ++ functionName
                        ++ "', but this function has already been defined."

                Just previousDefinitionRange ->
                    "You're trying to define a new function called '"
                        ++ functionName
                        ++ "', but this function has already been defined here:\n\n"
                        ++ extractFromString
                            source
                            previousDefinitionRange.start
                            previousDefinitionRange.end

        TypeAlreadyDefined typeName previousDefinitionRange ->
            "You're trying to define a new type called '"
                ++ typeName
                ++ "', but this type has already been defined here:\n\n"
                ++ extractFromString
                    source
                    previousDefinitionRange.start
                    previousDefinitionRange.end

        UnknownMetadata meta ->
            "'" ++ meta ++ ":' is not a known keyword in this context."

        InvalidModulePath path ->
            "'" ++ path ++ "' is not a valid module path. Note: Upper case characters are not allowed."

        ModuleIsEmpty ->
            "A module is required to contain at least one definition."

        BadDefinition name ->
            "'"
                ++ name
                ++ "' is not a valid definition. Expected either defmodule:, def:, defmulti:, defstruct: or defunion:"

        UnknownEscapeSequence seq ->
            "'"
                ++ seq
                ++ "' is not a valid escape sequence. Expected either \\n, \\t, \\\\ or \\\"."

        StringNotTerminated ->
            "This string never terminates. Expected to find a closing \"."

        IntegerBadLeadingZero ->
            "Integers cannot start with 0."

        IntegerTrailingUnderscore ->
            "Integers cannot end with an underscore."

        IntegerOutOfBounds ->
            "Integers must fit within a signed 32-bit number."
