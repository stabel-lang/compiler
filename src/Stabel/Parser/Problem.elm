module Stabel.Parser.Problem exposing
    ( Context(..)
    , Problem(..)
    , toString
    )

import Parser.Advanced exposing (DeadEnd)
import Stabel.Data.SourceLocation as SourceLocation
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
    | FunctionAlreadyDefined String (Maybe SourceLocationRange)
    | TypeAlreadyDefined String SourceLocationRange
    | UnknownMetadata String
    | InvalidModulePath String
    | ModuleIsEmpty
    | BadDefinition String


toString : String -> DeadEnd Context Problem -> String
toString source deadEnd =
    let
        contextExplination =
            contextStackExplination deadEnd

        lineOfProblem =
            deadEnd.row

        lineOfContext =
            firstContextRow deadEnd
                |> Maybe.withDefault lineOfProblem

        ( startLine, endLine ) =
            ( min lineOfProblem lineOfContext
            , max lineOfProblem lineOfContext
            )

        codeBlock =
            source
                |> String.trimRight
                |> String.lines
                |> List.indexedMap (\idx line -> ( idx + 1, line ))
                |> List.filter (\( idx, _ ) -> idx >= startLine && idx <= endLine)
                |> List.map
                    (\( idx, line ) ->
                        String.fromInt idx ++ " | " ++ line
                    )
                |> String.join "\n"

        problemDetail =
            problemToString source deadEnd.problem
    in
    [ contextExplination
    , codeBlock
    , problemDetail
    ]
        |> List.filter (not << String.isEmpty)
        |> String.join "\n\n"


contextStackExplination : DeadEnd Context Problem -> String
contextStackExplination deadEnd =
    case deadEnd.contextStack of
        contextFrame :: [] ->
            "I came across a problem while parsing the "
                ++ contextToString contextFrame.context

        contextFrame1 :: contextFrame2 :: _ ->
            "I came across a problem while parsing the "
                ++ contextToString contextFrame1.context
                ++ " of the "
                ++ contextToString contextFrame2.context

        _ ->
            "I came across a problem"


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

        MemberKeyword ->
            "member"

        ImplementationKeyword ->
            "implementation"

        ElseKeyword ->
            "else branch"


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
                        ++ SourceLocation.extractFromString source previousDefinitionRange

        TypeAlreadyDefined typeName previousDefinitionRange ->
            "You're trying to define a new type called '"
                ++ typeName
                ++ "', but this type has already been defined here:\n\n"
                ++ SourceLocation.extractFromString source previousDefinitionRange

        UnknownMetadata meta ->
            "'" ++ meta ++ "' is not a known keyword in this context."

        InvalidModulePath path ->
            "'" ++ path ++ "' is not a valid module path. Note: Upper case characters are not allowed."

        ModuleIsEmpty ->
            "A module is required to contain at least one definition."

        BadDefinition name ->
            "'"
                ++ name
                ++ "' is not a valid definition. Expected either def:, defmulti:, defstruct: or defunion:"
