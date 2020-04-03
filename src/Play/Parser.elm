module Play.Parser exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type)
import Play.Tokenizer as Token exposing (Token)
import Result.Extra as Result
import Set exposing (Set)


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type alias TypeDefinition =
    { name : String
    , members : List ( String, Type )
    }


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : List AstNode
    }


type AstNode
    = Integer Int
    | Word String
    | ConstructType String


parse : List Token -> Result () AST
parse tokens =
    let
        ( errors, ast ) =
            tokens
                |> gather isDefinition
                |> List.foldl parseDefinition
                    ( []
                    , { types = Dict.empty
                      , words = Dict.empty
                      }
                    )
    in
    case errors of
        [] ->
            Ok ast

        _ ->
            Err ()


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


definitionKeywords : Set String
definitionKeywords =
    Set.fromList [ "def", "deftype" ]


isDefinition : Token -> Bool
isDefinition token =
    case token of
        Token.Metadata value ->
            Set.member value definitionKeywords

        _ ->
            False


parseDefinition : List Token -> ( List (), AST ) -> ( List (), AST )
parseDefinition tokens ( errors, ast ) =
    case tokens of
        (Token.Metadata "def") :: (Token.Symbol wordName) :: rest ->
            case List.splitWhen (\token -> token == Token.Metadata "") rest of
                Nothing ->
                    ( () :: errors
                    , ast
                    )

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
                        ( [], Ok wordImpl ) ->
                            ( errors
                            , { ast
                                | words =
                                    Dict.insert wordName
                                        { name = wordName
                                        , metadata = metadata
                                        , implementation = wordImpl
                                        }
                                        ast.words
                              }
                            )

                        _ ->
                            ( () :: errors
                            , ast
                            )

        (Token.Metadata "deftype") :: (Token.Type typeName) :: [] ->
            ( errors
            , parseTypeDefinition typeName [] ast
            )

        (Token.Metadata "deftype") :: (Token.Type typeName) :: (Token.Metadata "") :: Token.ListStart :: rest ->
            case List.splitWhen (\t -> t == Token.ListEnd) rest of
                Just ( types, [ Token.ListEnd ] ) ->
                    case parseTypeMembers types [] of
                        Err () ->
                            ( () :: errors
                            , ast
                            )

                        Ok members ->
                            ( errors
                            , parseTypeDefinition typeName members ast
                            )

                _ ->
                    ( () :: errors
                    , ast
                    )

        _ ->
            ( () :: errors
            , ast
            )


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

        (Token.Metadata "type") :: values ->
            case List.splitWhen (\token -> token == Token.TypeSeperator) values of
                Just ( inputs, outputs ) ->
                    let
                        possibleInputTypes =
                            inputs
                                |> List.map parseType
                                |> Result.combine

                        possibleOutputTypes =
                            outputs
                                |> List.drop 1
                                |> List.map parseType
                                |> Result.combine
                    in
                    case ( possibleInputTypes, possibleOutputTypes ) of
                        ( Ok inputTypes, Ok outputTypes ) ->
                            ( errors
                            , { metadata | type_ = Just { input = inputTypes, output = outputTypes } }
                            )

                        _ ->
                            ( () :: errors
                            , metadata
                            )

                Nothing ->
                    ( () :: errors
                    , metadata
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


parseTypeDefinition : String -> List ( String, Type ) -> AST -> AST
parseTypeDefinition typeName members ast =
    let
        typeDef =
            { name = typeName
            , members = members
            }

        metadata =
            Metadata.default
                |> Metadata.withType (List.map Tuple.second members) [ Type.Custom typeName ]

        ctorDef =
            { name = ">" ++ typeName
            , metadata = metadata
            , implementation = [ ConstructType typeName ]
            }
    in
    { ast
        | types = Dict.insert typeName typeDef ast.types
        , words = Dict.insert ctorDef.name ctorDef ast.words
    }


parseType : Token -> Result () Type
parseType token =
    case token of
        Token.Type "Int" ->
            Ok Type.Int

        Token.Type name ->
            Ok <| Type.Custom name

        _ ->
            Err ()


parseTypeMembers : List Token -> List ( String, Type ) -> Result () (List ( String, Type ))
parseTypeMembers tokens acc =
    case tokens of
        [] ->
            Ok (List.reverse acc)

        (Token.Metadata name) :: ((Token.Type _) as typeToken) :: rest ->
            case parseType typeToken of
                Err () ->
                    Err ()

                Ok typeValue ->
                    parseTypeMembers rest (( name, typeValue ) :: acc)

        _ ->
            Err ()
