module Stabel.Parser exposing
    ( AST
    , AstNode(..)
    , FunctionDefinition
    , FunctionImplementation(..)
    , TypeDefinition
    , TypeDefinitionMembers(..)
    , TypeMatch(..)
    , run
    )

import Bitwise
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Random
import Set exposing (Set)
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature exposing (AssociatedFunctionSignature)
import Stabel.Parser.ModuleDefinition as ModuleDefinition exposing (ModuleDefinition)
import Stabel.Parser.Problem as Problem exposing (Context, Problem(..))
import Stabel.Parser.SourceLocation exposing (SourceLocation, SourceLocationRange)
import Stabel.Parser.Type
    exposing
        ( FunctionSignature
        , PossiblyQualifiedType(..)
        , PossiblyQualifiedTypeOrStackRange(..)
        )


type alias Parser a =
    Parser.Parser Context Problem a


type alias AST =
    { sourceReference : String
    , moduleDefinition : ModuleDefinition
    , types : Dict String TypeDefinition
    , functions : Dict String FunctionDefinition
    }


type alias TypeDefinition =
    { name : String
    , sourceLocation : SourceLocationRange
    , documentation : String
    , generics : List String
    , members : TypeDefinitionMembers
    }


type TypeDefinitionMembers
    = StructMembers (List ( String, PossiblyQualifiedType ))
    | UnionMembers (List PossiblyQualifiedType)


type alias FunctionDefinition =
    { name : String
    , typeSignature : AssociatedFunctionSignature
    , sourceLocationRange : Maybe SourceLocationRange
    , documentation : String
    , aliases : Dict String String
    , imports : Dict String (List String)
    , implementation : FunctionImplementation
    }


type FunctionImplementation
    = SoloImpl (List AstNode)
      -- TODO: Default branch should be a (Maybe (List AstNode))
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatchInt SourceLocationRange Int
    | TypeMatchType SourceLocationRange PossiblyQualifiedType (List ( String, TypeMatch ))


type AstNode
    = Integer SourceLocationRange Int
    | Function SourceLocationRange String
    | PackageFunction SourceLocationRange (List String) String
    | ExternalFunction SourceLocationRange (List String) String
    | FullyQualifiedFunction SourceLocationRange String
    | InlineFunction SourceLocationRange (List AstNode)
    | ConstructType String
    | GetMember String String
    | SetMember String String
    | ArrayLiteral SourceLocationRange (List AstNode)
    | StringLiteral SourceLocationRange String


run : String -> String -> Result (List (Parser.DeadEnd Context Problem)) AST
run ref sourceCode =
    Parser.run (parser ref) sourceCode



-- ATOMS


validSymbolChar : Char -> Bool
validSymbolChar c =
    not <| Set.member c invalidSymbolChars


invalidSymbolChars : Set Char
invalidSymbolChars =
    Set.union whitespaceChars specialChars


specialChars : Set Char
specialChars =
    Set.fromList
        [ ':'
        , '{'
        , '}'
        , '['
        , ']'
        , '('
        , ')'
        , '.'
        , '#'
        , '/'
        , '\''
        , '"'
        ]


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList
        [ ' '
        , '\n'
        , '\u{000D}'
        , '\t'
        ]



-- Int parsing


{-| The builtin int parser has a bug where it commits when it comes across an 'e'
-}
intParser : Parser Int
intParser =
    Parser.inContext Problem.IntegerLiteral
        (Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Token "0b" UnknownError)
                |= Parser.variable
                    { start = isBit
                    , inner = isBit
                    , reserved = Set.empty
                    , expecting = ExpectedBitInt
                    }
                |. whiteSpaceOrEnd
                |> Parser.andThen intBitParserHelper
            , Parser.succeed identity
                |. Parser.symbol (Token "0x" UnknownError)
                |= Parser.variable
                    { start = isHexDigit
                    , inner = isHexDigit
                    , reserved = Set.empty
                    , expecting = ExpectedHexInt
                    }
                |. whiteSpaceOrEnd
                |> Parser.andThen intHexParserHelper
            , Parser.succeed Tuple.pair
                |= Parser.variable
                    { start = Char.isDigit
                    , inner = \c -> Char.isDigit c || c == '_'
                    , reserved = Set.empty
                    , expecting = ExpectedInt
                    }
                |= Parser.oneOf
                    [ Parser.succeed True
                        |. Parser.symbol (Token "-" UnknownError)
                    , Parser.succeed False
                    ]
                |. whiteSpaceOrEnd
                |> Parser.andThen intParserHelper
            ]
        )


whiteSpaceOrEnd : Parser ()
whiteSpaceOrEnd =
    Parser.oneOf
        [ Parser.chompIf (\c -> Set.member c whitespaceChars) ExpectedWhitespace
        , Parser.succeed ()
            |. Parser.end ExpectedEndOfFile
        ]



-- Bit integers


isBit : Char -> Bool
isBit char =
    char == '0' || char == '1'


intBitParserHelper : String -> Parser Int
intBitParserHelper text =
    if String.length text > 32 then
        Parser.problem IntegerBitOutOfBounds

    else
        String.foldl bitCharFolder 0 text
            |> Parser.succeed


bitCharFolder : Char -> Int -> Int
bitCharFolder char num =
    num
        |> Bitwise.shiftLeftBy 1
        |> Bitwise.or (bitCharToNum char)


bitCharToNum : Char -> Int
bitCharToNum char =
    case char of
        '1' ->
            1

        '0' ->
            0

        _ ->
            -1



-- Hex integers


isHexDigit : Char -> Bool
isHexDigit char =
    List.member (Char.toUpper char) hexDigits


hexDigits : List Char
hexDigits =
    [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ]


intHexParserHelper : String -> Parser Int
intHexParserHelper text =
    if String.length text > 8 then
        Parser.problem IntegerHexOutOfBounds

    else
        String.foldl hexCharFolder 0 text
            |> Parser.succeed


hexCharFolder : Char -> Int -> Int
hexCharFolder char num =
    num
        |> Bitwise.shiftLeftBy 4
        |> Bitwise.or (hexCharToNum char)


hexCharToNum : Char -> Int
hexCharToNum char =
    List.elemIndex (Char.toUpper char) hexDigits
        |> Maybe.withDefault -1



-- Base 10 integers


intParserHelper : ( String, Bool ) -> Parser Int
intParserHelper ( text, isNegative ) =
    let
        digits =
            String.replace "_" "" text
    in
    if String.endsWith "_" text then
        Parser.problem IntegerTrailingUnderscore

    else if String.length digits > 1 && String.startsWith "0" digits then
        Parser.problem IntegerBadLeadingZero

    else
        case String.toInt digits of
            Just num ->
                let
                    actualNumber =
                        if isNegative then
                            num * -1

                        else
                            num
                in
                if actualNumber > Random.maxInt || actualNumber < Random.minInt then
                    Parser.problem IntegerOutOfBounds

                else
                    Parser.succeed actualNumber

            Nothing ->
                Parser.problem ExpectedInt


sourceLocationParser : Parser SourceLocation
sourceLocationParser =
    Parser.succeed SourceLocation
        |= Parser.getRow
        |= Parser.getCol


symbolParser : Parser String
symbolParser =
    symbolParserHelp
        (\c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars))


symbolImplParser : Parser String
symbolImplParser =
    symbolParserHelp
        (\c -> not (Char.isDigit c || Set.member c invalidSymbolChars))


symbolParserHelp : (Char -> Bool) -> Parser String
symbolParserHelp startPred =
    Parser.variable
        { start = startPred
        , inner = validSymbolChar
        , reserved = Set.empty
        , expecting = ExpectedSymbol
        }
        |. Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Token ":" ExpectedMetadata)
                |> Parser.andThen (\_ -> Parser.problem UnexpectedMetadata)
            , Parser.succeed identity
            ]
        |> Parser.backtrackable


symbolImplListParser : List String -> Parser (Parser.Step (List String) (List String))
symbolImplListParser symbols =
    Parser.oneOf
        [ Parser.succeed (\sym -> Parser.Loop (sym :: symbols))
            |= symbolImplParser
            |. noiseParser
        , Parser.succeed (Parser.Done <| List.reverse symbols)
        ]


modulePathStringParser : Parser String
modulePathStringParser =
    let
        joiner ( path, sym ) =
            String.join "/" (path ++ [ sym ])
    in
    Parser.oneOf
        [ Parser.succeed identity
            |= symbolImplParser
            |> Parser.andThen (\sym -> Parser.loop [ sym ] modulePathParser)
            |> Parser.map joiner
        , Parser.succeed (\res -> "/" ++ joiner res)
            |= Parser.loop [] modulePathParser
        ]


modulePathParser : List String -> Parser (Parser.Step (List String) ( List String, String ))
modulePathParser symbols =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: symbols))
            |. Parser.symbol (Token "/" ExpectedMetadata)
            |= symbolImplParser
        , Parser.succeed (Parser.Done (modulePathFinalizer symbols))
        ]


modulePathFinalizer : List String -> ( List String, String )
modulePathFinalizer symbols =
    case symbols of
        [] ->
            ( [], "" )

        [ only ] ->
            ( [], only )

        first :: rest ->
            ( List.reverse rest, first )


metadataParser : Parser String
metadataParser =
    metadataParserHelp Set.empty


definitionMetadataParser : Parser String
definitionMetadataParser =
    metadataParserHelp <| Set.fromList [ "def", "defmulti", "defstruct", "defunion" ]


metadataParserHelp : Set String -> Parser String
metadataParserHelp reserved =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = reserved
        , expecting = ExpectedMetadata
        }
        |. Parser.symbol (Token ":" ExpectedMetadata)


textParser : Parser String
textParser =
    Parser.chompWhile (\c -> not <| Set.member c whitespaceChars)
        |> Parser.getChompedString


stringParser : Parser String
stringParser =
    Parser.inContext Problem.StringLiteral
        (Parser.oneOf
            [ Parser.succeed stripMultilineStringWhitespace
                |. Parser.symbol (Token "\"\"\"" UnknownError)
                |= Parser.loop (Just "") multilineStringParserLoop
            , Parser.succeed identity
                |. Parser.symbol (Token "\"" UnknownError)
                |= Parser.loop (Just "") stringParserLoop
            ]
        )


stringParserLoop : Maybe String -> Parser (Parser.Step (Maybe String) String)
stringParserLoop maybeStr =
    case maybeStr of
        Nothing ->
            -- Work around bug in elm/parser
            Parser.problem StringNotTerminated

        Just str ->
            Parser.oneOf
                [ Parser.succeed (Parser.Done str)
                    |. Parser.symbol (Token "\"" UnknownError)
                , Parser.succeed (\char -> Parser.Loop <| Just <| str ++ String.fromChar char)
                    |. Parser.symbol (Token "\\" UnknownError)
                    |= Parser.oneOf
                        [ Parser.succeed '\n'
                            |. Parser.symbol (Token "n" UnknownError)
                        , Parser.succeed '\t'
                            |. Parser.symbol (Token "t" UnknownError)
                        , Parser.succeed '"'
                            |. Parser.symbol (Token "\"" UnknownError)
                        , Parser.succeed '\\'
                            |. Parser.symbol (Token "\\" UnknownError)
                        , Parser.succeed ()
                            |. Parser.chompIf (always True) UnknownError
                            |> Parser.getChompedString
                            |> Parser.andThen (\seq -> Parser.problem (UnknownEscapeSequence <| "\\" ++ seq))
                        ]

                -- Couldn't get the below to work in any other way :(
                , Parser.succeed (Parser.Loop Nothing)
                    |. Parser.end StringNotTerminated
                , Parser.succeed (Parser.Done str)
                    |. Parser.symbol (Token "\n" UnknownError)
                    |> Parser.andThen (\_ -> Parser.problem StringNotTerminated)
                , Parser.succeed ()
                    |. Parser.chompWhile (\c -> c /= '\n' && c /= '"' && c /= '\\')
                    |> Parser.getChompedString
                    |> Parser.map (\chompedStr -> Parser.Loop <| Just <| str ++ chompedStr)
                ]


multilineStringParserLoop : Maybe String -> Parser (Parser.Step (Maybe String) String)
multilineStringParserLoop maybeStr =
    case maybeStr of
        Nothing ->
            -- Work around bug in elm/parser
            Parser.problem StringNotTerminated

        Just str ->
            Parser.oneOf
                [ Parser.succeed (Parser.Done str)
                    |. Parser.symbol (Token "\"\"\"" UnknownError)

                -- Couldn't get the below to work in any other way :(
                , Parser.succeed (Parser.Loop Nothing)
                    |. Parser.end StringNotTerminated
                , Parser.succeed ()
                    |. Parser.chompWhile (\c -> c /= '"')
                    |> Parser.getChompedString
                    |> Parser.map (\chompedStr -> Parser.Loop <| Just <| str ++ chompedStr)
                ]


stripMultilineStringWhitespace : String -> String
stripMultilineStringWhitespace str =
    let
        noLeadingNewline =
            if String.startsWith "\n" str then
                String.dropLeft 1 str

            else
                str

        noEndingNewline =
            if String.endsWith "\n" noLeadingNewline then
                String.dropRight 1 noLeadingNewline

            else
                noLeadingNewline

        linesWithLeadingWhitespace =
            String.lines noEndingNewline
                |> List.map (\line -> ( countLeadingWhitespace line, line ))

        linesWithStrippedWhitespace =
            case linesWithLeadingWhitespace of
                [] ->
                    []

                ( maxWhitespace, _ ) :: _ ->
                    List.map
                        (stripMaxWhitespace maxWhitespace)
                        linesWithLeadingWhitespace
    in
    String.join "\n" linesWithStrippedWhitespace


countLeadingWhitespace : String -> Int
countLeadingWhitespace str =
    case String.uncons str of
        Just ( ' ', rest ) ->
            1 + countLeadingWhitespace rest

        _ ->
            0


stripMaxWhitespace : Int -> ( Int, String ) -> String
stripMaxWhitespace max ( whitespaceCount, line ) =
    String.dropLeft (min max whitespaceCount) line


genericParser : Parser String
genericParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol (Token "-" UnknownError)
            |> Parser.andThen (\_ -> Parser.problem ExpectedGeneric)
        , symbolParser
        ]
        |> Parser.backtrackable


typeNameParser : Parser String
typeNameParser =
    Parser.variable
        { start = Char.isUpper
        , inner = validSymbolChar
        , reserved = Set.empty
        , expecting = ExpectedType
        }


genericOrRangeParser : Parser PossiblyQualifiedTypeOrStackRange
genericOrRangeParser =
    let
        helper value =
            Parser.oneOf
                [ Parser.succeed (StackRange value)
                    |. Parser.symbol (Token "..." UnknownError)
                , Parser.succeed (NotStackRange <| Generic value)
                    |. Parser.chompIf (\c -> Set.member c whitespaceChars) ExpectedWhitespace
                ]
    in
    Parser.andThen helper genericParser


typeSignatureRefParser : Parser PossiblyQualifiedType
typeSignatureRefParser =
    Parser.oneOf
        [ Parser.succeed (\name -> LocalRef name [])
            |= typeNameParser
        , Parser.succeed (\( path, name ) -> FullyQualifiedRef ("/" ++ String.join "/" path ++ "/" ++ name) [])
            |. Parser.symbol (Token "//" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> ExternalRef path name [])
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> InternalRef path name [])
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= typeRefParser
            |. Parser.symbol (Token ")" ExpectedRightParen)
        ]


typeRefParser : Parser PossiblyQualifiedType
typeRefParser =
    Parser.oneOf
        [ Parser.succeed LocalRef
            |= typeNameParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> FullyQualifiedRef ("/" ++ String.join "/" path ++ "/" ++ name) binds)
            |. Parser.symbol (Token "//" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> ExternalRef path name binds)
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> InternalRef path name binds)
            |= Parser.loop [] modularizedTypeRefParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed Generic
            |= genericParser
            |. noiseParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= Parser.lazy (\_ -> typeRefParser)
            |. Parser.symbol (Token ")" ExpectedRightParen)
            |. noiseParser
        ]


typeMatchTypeParser : Parser PossiblyQualifiedType
typeMatchTypeParser =
    Parser.oneOf
        [ Parser.succeed (\name -> LocalRef name [])
            |= typeNameParser
        , Parser.succeed (\( path, name ) -> FullyQualifiedRef ("/" ++ String.join "/" path ++ "/" ++ name) [])
            |. Parser.symbol (Token "//" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> ExternalRef path name [])
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> InternalRef path name [])
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= typeRefParser
            |. Parser.symbol (Token ")" ExpectedRightParen)
        ]


modularizedTypeRefParser :
    List String
    -> Parser (Parser.Step (List String) ( List String, String ))
modularizedTypeRefParser reversedPath =
    let
        onType type_ =
            Parser.Done
                ( List.reverse reversedPath
                , type_
                )

        addToPath pathPiece =
            Parser.Loop (pathPiece :: reversedPath)
    in
    Parser.oneOf
        [ Parser.succeed onType
            |= typeNameParser
        , Parser.succeed addToPath
            |= symbolParser
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
        ]


typeOrGenericParser : List PossiblyQualifiedType -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
typeOrGenericParser types =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (LocalRef name [] :: types))
            |= typeNameParser
            |. noiseParser
        , Parser.succeed (\name -> Parser.Loop (Generic name :: types))
            |= genericParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


noiseParser : Parser ()
noiseParser =
    Parser.loop () noiseParserLoop


noiseParserLoop : () -> Parser (Parser.Step () ())
noiseParserLoop _ =
    Parser.oneOf
        [ Parser.succeed (Parser.Loop ())
            |. Parser.symbol (Token "#" UnknownError)
            |. Parser.chompWhile (\c -> c /= '\n')
        , Parser.succeed (Parser.Loop ())
            |. Parser.chompIf (\c -> Set.member c whitespaceChars) ExpectedWhitespace
            |. Parser.chompWhile (\c -> Set.member c whitespaceChars)
        , Parser.succeed (Parser.Done ())
        ]



-- Grammar


parser : String -> Parser AST
parser ref =
    let
        joinParseResults modDef ast =
            { ast | moduleDefinition = modDef }

        emptyAst =
            { sourceReference = ref
            , moduleDefinition = ModuleDefinition.Undefined
            , types = Dict.empty
            , functions = Dict.empty
            }

        checkIfEmpty ast =
            if Dict.isEmpty ast.types && Dict.isEmpty ast.functions then
                Parser.problem ModuleIsEmpty

            else
                Parser.succeed ast
    in
    Parser.succeed identity
        |. noiseParser
        |= Parser.oneOf
            [ Parser.succeed joinParseResults
                |= moduleDefinitionParser
                |. noiseParser
                |= Parser.loop emptyAst definitionParser
            , Parser.succeed (joinParseResults ModuleDefinition.Undefined)
                |= Parser.loop emptyAst definitionParser
            ]
        |. Parser.end ExpectedEndOfFile
        |> Parser.andThen checkIfEmpty


moduleDefinitionParser : Parser ModuleDefinition
moduleDefinitionParser =
    Parser.inContext Problem.ModuleDefinition
        (Parser.succeed identity
            |. Parser.keyword (Token "defmodule:" UnknownError)
            |. noiseParser
            |= Parser.loop ModuleDefinition.emptyDefinition moduleDefinitionMetaParser
            |> Parser.map ModuleDefinition.Defined
        )


moduleDefinitionMetaParser :
    ModuleDefinition.Definition
    -> Parser (Parser.Step ModuleDefinition.Definition ModuleDefinition.Definition)
moduleDefinitionMetaParser def =
    Parser.oneOf
        [ Parser.inContext Problem.AliasKeyword
            (Parser.succeed (\( value, alias ) -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
                |= aliasParser
                |. noiseParser
            )
        , Parser.inContext Problem.ImportKeyword
            (Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
                |. Parser.keyword (Token "import:" UnknownError)
                |. noiseParser
                |= modulePathStringParser
                |. noiseParser
                |= Parser.loop [] symbolImplListParser
                |. noiseParser
            )
        , Parser.inContext Problem.ExposingKeyword
            (Parser.succeed (\exposings -> Parser.Loop { def | exposes = exposings })
                |. Parser.keyword (Token "exposing:" UnknownError)
                |. noiseParser
                |= (Parser.loop [] symbolImplListParser |> Parser.map Set.fromList)
                |. noiseParser
            )
        , Parser.succeed (\str -> Parser.Loop { def | documentation = str })
            |. Parser.keyword (Token "doc:" UnknownError)
            |. noiseParser
            |= stringParser
            |. noiseParser
        , Parser.succeed UnknownMetadata
            |= metadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
            |. Parser.keyword (Token ":" UnknownError)
            |. noiseParser
        ]


aliasParser : Parser ( String, String )
aliasParser =
    Parser.succeed aliasParserHelp
        |. Parser.keyword (Token "alias:" UnknownError)
        |. noiseParser
        |= modulePathStringParser
        |. noiseParser
        |= Parser.oneOf
            [ Parser.succeed Just
                |= symbolParser
            , Parser.succeed Nothing
            ]


aliasParserHelp : String -> Maybe String -> ( String, String )
aliasParserHelp modulePath maybeAlias =
    case maybeAlias of
        Just alias ->
            ( modulePath, alias )

        Nothing ->
            ( modulePath
            , modulePath
                |> String.split "/"
                |> List.last
                -- will never happen
                |> Maybe.withDefault ""
            )


definitionParser : AST -> Parser (Parser.Step AST AST)
definitionParser ast =
    let
        insertFunction funcDef =
            { ast | functions = Dict.insert funcDef.name funcDef ast.functions }
                |> Parser.Loop
                |> Parser.succeed

        insertType typeDef =
            let
                typeName =
                    typeDef.name

                typeFunctions =
                    generateDefaultFunctionsForType typeDef
            in
            { ast
                | types = Dict.insert typeName typeDef ast.types
                , functions = Dict.union (Dict.fromListBy .name typeFunctions) ast.functions
            }
                |> Parser.Loop
                |> Parser.succeed
    in
    Parser.oneOf
        [ Parser.succeed Tuple.pair
            |= sourceLocationParser
            |. Parser.keyword (Token "def:" UnknownError)
            |. noiseParser
            |= symbolParser
            |> Parser.andThen (functionDefinitionParser ast.functions)
            |> Parser.andThen insertFunction
        , Parser.succeed Tuple.pair
            |= sourceLocationParser
            |. Parser.keyword (Token "defmulti:" UnknownError)
            |. noiseParser
            |= symbolParser
            |> Parser.andThen (multiFunctionDefinitionParser ast.functions)
            |> Parser.andThen insertFunction
        , Parser.succeed Tuple.pair
            |= sourceLocationParser
            |. Parser.keyword (Token "defstruct:" UnknownError)
            |. noiseParser
            |= typeNameParser
            |> Parser.andThen (typeDefinitionParser ast.types ast.functions)
            |> Parser.andThen insertType
        , Parser.succeed Tuple.pair
            |= sourceLocationParser
            |. Parser.keyword (Token "defunion:" UnknownError)
            |. noiseParser
            |= typeNameParser
            |> Parser.andThen (unionTypeDefinitionParser ast.types)
            |> Parser.andThen insertType
        , Parser.succeed BadDefinition
            |= textParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done ast)
        ]


generateDefaultFunctionsForType : TypeDefinition -> List FunctionDefinition
generateDefaultFunctionsForType typeDef =
    case typeDef.members of
        UnionMembers _ ->
            []

        StructMembers typeMembers ->
            let
                typeOfType =
                    typeDef.generics
                        |> List.map Generic
                        |> LocalRef typeDef.name
                        |> NotStackRange

                ctorDef =
                    { name =
                        if List.isEmpty typeMembers then
                            typeDef.name ++ ">"

                        else
                            ">" ++ typeDef.name
                    , typeSignature =
                        AssociatedFunctionSignature.Verified
                            { input =
                                List.map (NotStackRange << Tuple.second) typeMembers
                            , output = [ typeOfType ]
                            }
                    , sourceLocationRange = Nothing
                    , documentation = ""
                    , aliases = Dict.empty
                    , imports = Dict.empty
                    , implementation =
                        SoloImpl [ ConstructType typeDef.name ]
                    }

                setterGetterPair ( memberName, memberType ) =
                    [ { name = ">" ++ memberName
                      , typeSignature =
                            AssociatedFunctionSignature.Verified
                                { input = [ typeOfType, NotStackRange memberType ]
                                , output = [ typeOfType ]
                                }
                      , sourceLocationRange = Nothing
                      , documentation = ""
                      , aliases = Dict.empty
                      , imports = Dict.empty
                      , implementation =
                            SoloImpl
                                [ SetMember typeDef.name memberName ]
                      }
                    , { name = memberName ++ ">"
                      , typeSignature =
                            AssociatedFunctionSignature.Verified
                                { input = [ typeOfType ]
                                , output = [ NotStackRange memberType ]
                                }
                      , sourceLocationRange = Nothing
                      , documentation = ""
                      , aliases = Dict.empty
                      , imports = Dict.empty
                      , implementation =
                            SoloImpl
                                [ GetMember typeDef.name memberName ]
                      }
                    ]
            in
            typeMembers
                |> List.concatMap setterGetterPair
                |> (::) ctorDef


functionDefinitionParser : Dict String FunctionDefinition -> ( SourceLocation, String ) -> Parser FunctionDefinition
functionDefinitionParser definedFunctions ( startLocation, name ) =
    let
        joinParseResults def endLocation =
            { def
                | sourceLocationRange =
                    Just
                        { start = startLocation
                        , end = endLocation
                        }
            }

        emptyDef =
            { name = name
            , typeSignature = AssociatedFunctionSignature.NotProvided
            , sourceLocationRange = Nothing
            , documentation = ""
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation = SoloImpl []
            }
    in
    Parser.inContext (Problem.FunctionDefinition startLocation name) <|
        case Dict.get name definedFunctions of
            Just previousDefinition ->
                Parser.problem <|
                    Problem.FunctionAlreadyDefined name previousDefinition.sourceLocationRange

            _ ->
                Parser.succeed joinParseResults
                    |. noiseParser
                    |= Parser.loop emptyDef functionMetadataParser
                    |= sourceLocationParser


functionMetadataParser : FunctionDefinition -> Parser (Parser.Step FunctionDefinition FunctionDefinition)
functionMetadataParser def =
    Parser.oneOf
        [ Parser.inContext Problem.TypeKeyword <|
            Parser.succeed (\typeSign -> Parser.Loop { def | typeSignature = AssociatedFunctionSignature.UserProvided typeSign })
                |. Parser.keyword (Token "type:" UnknownError)
                |. noiseParser
                |= typeSignatureParser
        , Parser.inContext Problem.DocKeyword <|
            Parser.succeed (\str -> Parser.Loop { def | documentation = str })
                |. Parser.keyword (Token "doc:" UnknownError)
                |. noiseParser
                |= stringParser
                |. noiseParser
        , Parser.inContext Problem.AliasKeyword <|
            Parser.succeed (\( value, alias ) -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
                |= aliasParser
                |. noiseParser
        , Parser.inContext Problem.ImportKeyword <|
            Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
                |. Parser.keyword (Token "import:" UnknownError)
                |. noiseParser
                |= modulePathStringParser
                |. noiseParser
                |= Parser.loop [] symbolImplListParser
                |. noiseParser
        , Parser.inContext Problem.ImplementationKeyword <|
            Parser.succeed (\impl -> Parser.Loop { def | implementation = SoloImpl impl })
                |. Parser.keyword (Token ":" UnknownError)
                |. noiseParser
                |= implementationParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
        ]


multiFunctionDefinitionParser : Dict String FunctionDefinition -> ( SourceLocation, String ) -> Parser FunctionDefinition
multiFunctionDefinitionParser definedFunctions ( startLocation, name ) =
    let
        joinParseResults def endLocation =
            { def
                | sourceLocationRange =
                    Just
                        { start = startLocation
                        , end = endLocation
                        }
                , implementation = reverseWhens def.implementation
            }

        emptyDef =
            { name = name
            , typeSignature = AssociatedFunctionSignature.NotProvided
            , sourceLocationRange = Nothing
            , documentation = ""
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation = SoloImpl []
            }

        reverseWhens implementation =
            case implementation of
                SoloImpl _ ->
                    implementation

                MultiImpl whens impl ->
                    MultiImpl (List.reverse whens) impl
    in
    Parser.inContext (Problem.MultifunctionDefinition startLocation name) <|
        case Dict.get name definedFunctions of
            Just previousDefinition ->
                Parser.problem <|
                    Problem.FunctionAlreadyDefined name previousDefinition.sourceLocationRange

            _ ->
                Parser.succeed joinParseResults
                    |. noiseParser
                    |= Parser.loop emptyDef multiFunctionMetadataParser
                    |= sourceLocationParser


multiFunctionMetadataParser : FunctionDefinition -> Parser (Parser.Step FunctionDefinition FunctionDefinition)
multiFunctionMetadataParser def =
    let
        addWhenImpl impl =
            case def.implementation of
                MultiImpl whens default ->
                    MultiImpl (impl :: whens) default

                SoloImpl default ->
                    MultiImpl [ impl ] default

        setDefaultImpl impl =
            case def.implementation of
                MultiImpl whens _ ->
                    MultiImpl whens impl

                SoloImpl _ ->
                    MultiImpl [] impl
    in
    Parser.oneOf
        [ Parser.inContext Problem.TypeKeyword <|
            Parser.succeed (\typeSign -> Parser.Loop { def | typeSignature = AssociatedFunctionSignature.UserProvided typeSign })
                |. Parser.keyword (Token "type:" UnknownError)
                |. noiseParser
                |= typeSignatureParser
        , Parser.inContext Problem.DocKeyword <|
            Parser.succeed (\str -> Parser.Loop { def | documentation = str })
                |. Parser.keyword (Token "doc:" UnknownError)
                |. noiseParser
                |= stringParser
                |. noiseParser
        , Parser.inContext Problem.ElseKeyword <|
            Parser.succeed (\impl -> Parser.Loop { def | implementation = setDefaultImpl impl })
                |. Parser.keyword (Token "else:" UnknownError)
                |. noiseParser
                |= implementationParser
        , Parser.inContext Problem.AliasKeyword <|
            Parser.succeed (\( value, alias ) -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
                |= aliasParser
                |. noiseParser
        , Parser.inContext Problem.ImportKeyword <|
            Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
                |. Parser.keyword (Token "import:" UnknownError)
                |. noiseParser
                |= modulePathStringParser
                |. noiseParser
                |= Parser.loop [] symbolImplListParser
                |. noiseParser
        , Parser.inContext Problem.ImplementationKeyword <|
            Parser.succeed (\type_ impl -> Parser.Loop { def | implementation = addWhenImpl ( type_, impl ) })
                |. Parser.keyword (Token ":" UnknownError)
                |. noiseParser
                |= typeMatchParser
                |. noiseParser
                |= implementationParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
        ]


typeDefinitionParser :
    Dict String TypeDefinition
    -> Dict String FunctionDefinition
    -> ( SourceLocation, String )
    -> Parser TypeDefinition
typeDefinitionParser definedTypes definedFunctions ( startLocation, typeName ) =
    let
        ctor generics result endLocation =
            { name = typeName
            , sourceLocation = SourceLocationRange startLocation endLocation
            , documentation = result.documentation
            , generics = generics
            , members = StructMembers result.members
            }
    in
    Parser.inContext (Problem.StructDefinition startLocation typeName) <|
        case Dict.get typeName definedTypes of
            Just previousDefinition ->
                Parser.problem <|
                    TypeAlreadyDefined
                        typeName
                        previousDefinition.sourceLocation

            Nothing ->
                Parser.succeed ctor
                    |. noiseParser
                    |= Parser.loop [] typeGenericParser
                    |= Parser.loop
                        { documentation = ""
                        , members = []
                        }
                        (typeMemberParser definedFunctions)
                    |= sourceLocationParser


typeGenericParser : List String -> Parser (Parser.Step (List String) (List String))
typeGenericParser generics =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: generics))
            |= genericParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse generics))
        ]


type alias StructMembersParserResult =
    { documentation : String
    , members : List ( String, PossiblyQualifiedType )
    }


typeMemberParser :
    Dict String FunctionDefinition
    -> StructMembersParserResult
    ->
        Parser
            (Parser.Step
                StructMembersParserResult
                StructMembersParserResult
            )
typeMemberParser functions result =
    let
        alreadyDefinedCheck name =
            let
                getterName =
                    name ++ ">"

                setterName =
                    ">" ++ name
            in
            case ( Dict.get getterName functions, Dict.get setterName functions ) of
                ( Just getter, _ ) ->
                    Parser.problem <|
                        Problem.FunctionAlreadyDefined
                            getter.name
                            getter.sourceLocationRange

                ( _, Just setter ) ->
                    Parser.problem <|
                        Problem.FunctionAlreadyDefined
                            setter.name
                            setter.sourceLocationRange

                ( Nothing, Nothing ) ->
                    Parser.succeed name
    in
    Parser.oneOf
        [ Parser.inContext Problem.MemberKeyword <|
            Parser.succeed (\name type_ -> Parser.Loop { result | members = ( name, type_ ) :: result.members })
                |. Parser.symbol (Token ":" UnknownError)
                |. noiseParser
                |= Parser.andThen alreadyDefinedCheck symbolParser
                |. noiseParser
                |= typeRefParser
        , Parser.inContext Problem.DocKeyword <|
            Parser.succeed (\str -> Parser.Loop { result | documentation = str })
                |. Parser.keyword (Token "doc:" UnknownError)
                |. noiseParser
                |= stringParser
                |. noiseParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done { result | members = List.reverse result.members })
        ]


unionTypeDefinitionParser : Dict String TypeDefinition -> ( SourceLocation, String ) -> Parser TypeDefinition
unionTypeDefinitionParser definedTypes ( startLocation, typeName ) =
    let
        ctor generics result endLocation =
            { name = typeName
            , sourceLocation = SourceLocationRange startLocation endLocation
            , documentation = result.documentation
            , generics = generics
            , members = UnionMembers result.types
            }
    in
    Parser.inContext (Problem.UnionDefinition startLocation typeName) <|
        case Dict.get typeName definedTypes of
            Just previousDefinition ->
                Parser.problem <|
                    TypeAlreadyDefined
                        typeName
                        previousDefinition.sourceLocation

            Nothing ->
                Parser.succeed ctor
                    |. noiseParser
                    |= Parser.loop [] typeGenericParser
                    |= Parser.loop
                        { documentation = ""
                        , types = []
                        }
                        unionTypeMemberParser
                    |= sourceLocationParser


type alias UnionMembersParserResult =
    { documentation : String
    , types : List PossiblyQualifiedType
    }


unionTypeMemberParser :
    UnionMembersParserResult
    -> Parser (Parser.Step UnionMembersParserResult UnionMembersParserResult)
unionTypeMemberParser result =
    Parser.oneOf
        [ Parser.inContext Problem.MemberKeyword <|
            Parser.succeed (\type_ -> Parser.Loop { result | types = type_ :: result.types })
                |. Parser.symbol (Token ":" UnknownError)
                |. noiseParser
                |= typeRefParser
        , Parser.inContext Problem.DocKeyword <|
            Parser.succeed (\str -> Parser.Loop { result | documentation = str })
                |. Parser.keyword (Token "doc:" UnknownError)
                |. noiseParser
                |= stringParser
                |. noiseParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done { result | types = List.reverse result.types })
        ]


typeSignatureParser : Parser FunctionSignature
typeSignatureParser =
    Parser.succeed (\input output -> { input = input, output = output })
        |= Parser.loop [] typeLoopParser
        |. Parser.symbol (Token "--" ExpectedTypeSeperator)
        |. noiseParser
        |= Parser.loop [] typeLoopParser


typeLoopParser :
    List PossiblyQualifiedTypeOrStackRange
    ->
        Parser
            (Parser.Step
                (List PossiblyQualifiedTypeOrStackRange)
                (List PossiblyQualifiedTypeOrStackRange)
            )
typeLoopParser reverseTypes =
    let
        step type_ =
            Parser.Loop (type_ :: reverseTypes)
    in
    Parser.oneOf
        [ Parser.succeed step
            |= genericOrRangeParser
            |. noiseParser
        , Parser.succeed (NotStackRange >> step)
            |= typeSignatureRefParser
            |. noiseParser
        , Parser.succeed (\functionType -> step (NotStackRange <| FunctionType functionType))
            |. Parser.symbol (Token "[" ExpectedLeftBracket)
            |. noiseParser
            |= typeSignatureParser
            |. Parser.symbol (Token "]" ExpectedRightBracket)
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse reverseTypes))
        ]


typeMatchParser : Parser TypeMatch
typeMatchParser =
    Parser.oneOf
        [ Parser.succeed (\startLoc int endLoc -> TypeMatchInt (SourceLocationRange startLoc endLoc) int)
            |= sourceLocationParser
            |= intParser
            |= sourceLocationParser
        , Parser.succeed typeMatchTypeHelper
            |= sourceLocationParser
            |= typeMatchTypeParser
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol (Token "(" ExpectedLeftParen)
                    |. noiseParser
                    |= Parser.loop [] typeMatchConditionParser
                    |. Parser.symbol (Token ")" ExpectedRightParen)
                , Parser.succeed []
                ]
            |= sourceLocationParser
        ]


typeMatchTypeHelper : SourceLocation -> PossiblyQualifiedType -> List ( String, TypeMatch ) -> SourceLocation -> TypeMatch
typeMatchTypeHelper startLoc type_ conds endLoc =
    TypeMatchType (SourceLocationRange startLoc endLoc) type_ conds


typeMatchConditionParser : List ( String, TypeMatch ) -> Parser (Parser.Step (List ( String, TypeMatch )) (List ( String, TypeMatch )))
typeMatchConditionParser nodes =
    Parser.oneOf
        [ Parser.succeed (\name value -> Parser.Loop (( name, value ) :: nodes))
            |= symbolParser
            |. noiseParser
            |= typeMatchParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse nodes))
        ]


implementationParser : Parser (List AstNode)
implementationParser =
    Parser.loop [] implementationParserHelp


implementationParserHelp : List AstNode -> Parser (Parser.Step (List AstNode) (List AstNode))
implementationParserHelp nodes =
    Parser.oneOf
        [ Parser.succeed (\node -> Parser.Loop (node :: nodes))
            |= nodeParser
            |. noiseParser
        , Parser.succeed (\startLoc quotImpl endLoc -> Parser.Loop (InlineFunction (SourceLocationRange startLoc endLoc) quotImpl :: nodes))
            |= sourceLocationParser
            |. Parser.symbol (Token "[" ExpectedLeftBracket)
            |. noiseParser
            |= implementationParser
            |. Parser.symbol (Token "]" ExpectedRightBracket)
            |= sourceLocationParser
            |. noiseParser
        , Parser.inContext Problem.ArrayLiteral
            (Parser.succeed (\startLoc arrContent endLoc -> Parser.Loop (ArrayLiteral (SourceLocationRange startLoc endLoc) arrContent :: nodes))
                |= sourceLocationParser
                |. Parser.symbol (Token "{" ExpectedLeftCurly)
                |. noiseParser
                |= implementationParser
                |. Parser.symbol (Token "}" ExpectedRightCurly)
                |= sourceLocationParser
                |. noiseParser
            )
        , Parser.succeed (\startLoc strContent endLoc -> Parser.Loop (StringLiteral (SourceLocationRange startLoc endLoc) strContent :: nodes))
            |= sourceLocationParser
            |= stringParser
            -- stringParser chomps the final "
            |= sourceLocationParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse nodes))
        ]


nodeParser : Parser AstNode
nodeParser =
    Parser.oneOf
        [ Parser.succeed (\startLoc value endLoc -> Integer (SourceLocationRange startLoc endLoc) value)
            |= sourceLocationParser
            |= intParser
            |= sourceLocationParser
        , Parser.succeed (\startLoc builder endLoc -> builder (SourceLocationRange startLoc endLoc))
            |= sourceLocationParser
            |= qualifiedSymbolImplParser
            |= sourceLocationParser
        ]


qualifiedSymbolImplParser : Parser (SourceLocationRange -> AstNode)
qualifiedSymbolImplParser =
    let
        fullyQualifiedBuilder firstPath ( restPath, functionName ) =
            let
                path =
                    firstPath :: restPath

                ref =
                    "/" ++ String.join "/" path ++ "/" ++ functionName
            in
            if checkForUpperCaseLetterInPath path then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path

            else if List.length path == 0 then
                Parser.problem <| InvalidModulePath <| ref

            else
                Parser.succeed <|
                    \loc ->
                        FullyQualifiedFunction loc ref

        externalBuilder ( path, reference ) =
            if checkForUpperCaseLetterInPath path then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path

            else if List.length path == 0 then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path ++ "/" ++ reference

            else
                Parser.succeed <|
                    \loc ->
                        ExternalFunction loc path reference

        internalBuilder firstSymbol (( partialPath, reference ) as modulePathResult) =
            let
                path =
                    firstSymbol :: partialPath
            in
            if checkForUpperCaseLetterInPath path && partialPath /= [] then
                Parser.problem <| InvalidModulePath <| String.join "/" path

            else
                Parser.succeed <|
                    \loc ->
                        if modulePathResult == ( [], "" ) then
                            Function loc firstSymbol

                        else
                            PackageFunction loc path reference

        checkForUpperCaseLetterInPath path =
            List.any (String.any Char.isUpper) path
    in
    Parser.oneOf
        [ Parser.succeed internalBuilder
            |= symbolImplParser
            |= Parser.loop [] modulePathParser
            |> Parser.andThen identity
        , Parser.succeed fullyQualifiedBuilder
            |. Parser.token (Token "//" ExpectedForwardSlash)
            |= symbolImplParser
            |= Parser.loop [] modulePathParser
            |> Parser.andThen identity
        , Parser.succeed identity
            |= Parser.loop [] modulePathParser
            |> Parser.andThen externalBuilder
        ]
