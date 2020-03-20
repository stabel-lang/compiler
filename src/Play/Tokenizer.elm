module Play.Tokenizer exposing (..)


type Token
    = Integer Int
    | Symbol String
    | Metadata String


tokenize : String -> Result () (List Token)
tokenize sourceCode =
    sourceCode
        |> String.words
        |> List.map recognizeToken
        |> tokenListOrError


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


tokenListOrError : List (Result () Token) -> Result () (List Token)
tokenListOrError potentialTokens =
    tokenListOrErrorHelper potentialTokens []


tokenListOrErrorHelper : List (Result () Token) -> List Token -> Result () (List Token)
tokenListOrErrorHelper potentialTokens tokens =
    case potentialTokens of
        [] ->
            Ok (List.reverse tokens)

        (Err ()) :: _ ->
            Err ()

        (Ok token) :: remaining ->
            tokenListOrErrorHelper remaining (token :: tokens)
