module Play.Tokenizer exposing (..)

import Result.Extra as Result


type Token
    = Integer Int
    | Symbol String
    | Metadata String


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
            if String.endsWith ":" word then
                word
                    |> String.dropRight 1
                    |> Metadata
                    |> Ok

            else
                Ok (Symbol word)
