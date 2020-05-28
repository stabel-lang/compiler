module Play.Tokenizer exposing
    ( Token(..)
    , tokenize
    )

import Parser exposing ((|.), (|=), Parser)
import Result.Extra as Result
import Set exposing (Set)


type Token
    = Integer Int
    | Symbol String
    | Metadata String
    | Type String
    | TypeSeperator
    | ListStart
    | ListEnd
    | QuoteStart
    | QuoteStop
    | PatternMatchStart String
    | ParenStop


tokenize : String -> Result () (List Token)
tokenize sourceCode =
    Parser.run parser sourceCode
        |> Result.mapError (Debug.log "err")
        |> Result.mapError (always ())


parser : Parser (List Token)
parser =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.loop [] parserHelp
        |. Parser.spaces



--|. Parser.end


parserHelp : List Token -> Parser (Parser.Step (List Token) (List Token))
parserHelp reverseTokens =
    Parser.oneOf
        [ Parser.succeed (\token -> Parser.Loop (token :: reverseTokens))
            |= tokenParser
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse reverseTokens))
        ]


tokenParser : Parser Token
tokenParser =
    Parser.oneOf
        [ intParser
            |> Parser.map Integer
        , Parser.succeed (Metadata "")
            |. Parser.symbol ":"
        , Parser.succeed ListStart
            |. Parser.symbol "{"
        , Parser.succeed ListEnd
            |. Parser.symbol "}"
        , Parser.succeed QuoteStart
            |. Parser.symbol "["
        , Parser.succeed QuoteStop
            |. Parser.symbol "]"
        , Parser.succeed ParenStop
            |. Parser.symbol ")"
        , Parser.succeed TypeSeperator
            |. Parser.symbol "--"
        , typeParser
        , symbolParser
        ]


intParser : Parser Int
intParser =
    -- The builtin int parser has a bug where it commits when it comes across an 'e'
    let
        helper text =
            case String.toInt text of
                Just num ->
                    Parser.succeed num

                Nothing ->
                    Parser.problem "Not a valid integer"
    in
    Parser.variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.empty
        }
        |> Parser.andThen helper


typeParser : Parser Token
typeParser =
    let
        helper text =
            Parser.oneOf
                [ Parser.succeed (PatternMatchStart text)
                    |. Parser.chompIf (\c -> c == '(')
                , Parser.succeed (Type text)
                ]
    in
    Parser.variable
        { start = Char.isUpper
        , inner = validSymbolChar
        , reserved = Set.empty
        }
        |> Parser.andThen helper


symbolParser : Parser Token
symbolParser =
    let
        helper text =
            Parser.oneOf
                [ Parser.succeed (Metadata text)
                    |. Parser.chompIf (\c -> c == ':')
                , Parser.succeed (Symbol text)
                ]
    in
    Parser.variable
        { start = validSymbolChar
        , inner = validSymbolChar
        , reserved = Set.empty
        }
        |> Parser.andThen helper


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
        ]


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList
        [ ' '
        , '\n'
        , '\u{000D}'
        , '\t'
        ]
