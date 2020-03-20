module Play.Parser exposing (..)

import List.Extra as List
import Play.Tokenizer as Token exposing (Token)


type alias Definition =
    { name : String
    , metadata : List ( String, List AstNode )
    , implementation : List AstNode
    }


type AstNode
    = Integer Int
    | Word String


parse : List Token -> Result () (List Definition)
parse tokens =
    tokens
        |> gather isDefinition
        |> List.map parseDefinition
        |> listOrError


gather : (a -> Bool) -> List a -> List (List a)
gather pred tokens =
    gatherHelp pred tokens []


gatherHelp : (a -> Bool) -> List a -> List (List a) -> List (List a)
gatherHelp pred tokens acc =
    case tokens of
        [] ->
            List.reverse acc

        first :: rest ->
            let
                tilNextDefinition =
                    if pred first then
                        first :: List.takeWhile (not << pred) rest

                    else
                        List.takeWhile (not << pred) tokens

                remainingTokens =
                    List.drop (List.length tilNextDefinition) tokens
            in
            gatherHelp pred remainingTokens (tilNextDefinition :: acc)


isDefinition : Token -> Bool
isDefinition token =
    case token of
        Token.Metadata "def" ->
            True

        _ ->
            False


parseDefinition : List Token -> Result () Definition
parseDefinition tokens =
    case tokens of
        (Token.Metadata "def") :: (Token.Symbol wordName) :: rest ->
            case List.splitWhen (\token -> token == Token.Metadata "") rest of
                Nothing ->
                    Err ()

                Just ( meta, impl ) ->
                    let
                        parsedMeta =
                            meta
                                |> gather isMeta
                                |> List.map parseMeta
                                |> listOrError

                        parsedImpl =
                            impl
                                |> List.drop 1
                                |> List.map parseAstNode
                                |> listOrError
                    in
                    case ( parsedMeta, parsedImpl ) of
                        ( Ok meta_, Ok ast ) ->
                            Ok
                                { name = wordName
                                , metadata = meta_
                                , implementation = ast
                                }

                        _ ->
                            Err ()

        _ ->
            Err ()


isMeta : Token -> Bool
isMeta token =
    case token of
        Token.Metadata _ ->
            True

        _ ->
            False


parseMeta : List Token -> Result () ( String, List AstNode )
parseMeta tokens =
    case tokens of
        (Token.Metadata keyName) :: rest ->
            let
                parsedValues =
                    rest
                        |> List.map parseAstNode
                        |> listOrError
            in
            case parsedValues of
                Err () ->
                    Err ()

                Ok values ->
                    Ok ( keyName, values )

        _ ->
            Err ()


parseAstNode : Token -> Result () AstNode
parseAstNode token =
    case token of
        Token.Integer value ->
            Ok (Integer value)

        Token.Symbol value ->
            Ok (Word value)

        _ ->
            Err ()


listOrError : List (Result () a) -> Result () (List a)
listOrError list =
    listOrErrorHelper list []


listOrErrorHelper : List (Result () a) -> List a -> Result () (List a)
listOrErrorHelper potentialTokens tokens =
    case potentialTokens of
        [] ->
            Ok (List.reverse tokens)

        (Err ()) :: _ ->
            Err ()

        (Ok token) :: remaining ->
            listOrErrorHelper remaining (token :: tokens)
