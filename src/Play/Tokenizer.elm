module Play.Tokenizer exposing (..)

import Result.Extra as Result


type Token
    = Integer Int
    | Symbol String
    | Metadata String
    | Type String
    | TypeSeperator


tokenize : String -> Result () (List Token)
tokenize sourceCode =
    sourceCode
        |> String.words
        |> List.map recognizeToken
        |> Result.combine


recognizeToken : String -> Result () Token
recognizeToken word =
    case String.toInt word of
        Just value ->
            Ok (Integer value)

        Nothing ->
            if stringStartsWithUpper word then
                Ok (Type word)

            else if String.endsWith ":" word then
                word
                    |> String.dropRight 1
                    |> Metadata
                    |> Ok

            else if word == "--" then
                Ok TypeSeperator

            else
                Ok (Symbol word)


stringStartsWithUpper : String -> Bool
stringStartsWithUpper str =
    case String.uncons str of
        Just ( firstLetter, _ ) ->
            Char.isUpper firstLetter

        Nothing ->
            False
