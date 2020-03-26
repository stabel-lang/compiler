module Play.Parser exposing (..)

import List.Extra as List
import Play.Tokenizer as Token exposing (Token)
import Result.Extra as Result


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
        |> Result.combine


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
                                |> Result.combine

                        parsedImpl =
                            impl
                                |> List.drop 1
                                |> List.map parseAstNode
                                |> Result.combine
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
                        |> Result.combine
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
