module Play.Parser exposing (..)

import List.Extra as List
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Tokenizer as Token exposing (Token)
import Result.Extra as Result


type alias Definition =
    { name : String
    , metadata : Metadata
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
                        ( metaParseErrors, metadata ) =
                            meta
                                |> gather isMeta
                                |> List.foldl parseMeta ( [], Metadata.default )

                        parsedImpl =
                            impl
                                |> List.drop 1
                                |> List.map parseAstNode
                                |> Result.combine
                    in
                    case ( metaParseErrors, parsedImpl ) of
                        ( [], Ok ast ) ->
                            Ok
                                { name = wordName
                                , metadata = metadata
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


parseMeta : List Token -> ( List (), Metadata ) -> ( List (), Metadata )
parseMeta tokens ( errors, metadata ) =
    case tokens of
        [ Token.Metadata "entry", value ] ->
            if value /= Token.Symbol "true" then
                ( () :: errors
                , metadata
                )

            else
                ( errors
                , { metadata | isEntryPoint = True }
                )

        _ ->
            ( () :: errors
            , metadata
            )


parseAstNode : Token -> Result () AstNode
parseAstNode token =
    case token of
        Token.Integer value ->
            Ok (Integer value)

        Token.Symbol value ->
            Ok (Word value)

        _ ->
            Err ()
