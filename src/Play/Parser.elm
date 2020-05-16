module Play.Parser exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
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


type TypeDefinition
    = CustomTypeDef String (List ( String, Type ))
    | UnionTypeDef String (List Type)


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatch Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type AstNode
    = Integer Int
    | Word String
    | ConstructType String
    | GetMember String String
    | SetMember String String
    | Quotation (List AstNode)


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
    Set.fromList
        [ "def"
        , "deftype"
        , "defunion"
        , "defmulti"
        ]


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
                                |> parseAstNodes []
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
                                        , implementation = SoloImpl wordImpl
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

        (Token.Metadata "defunion") :: (Token.Type typeName) :: (Token.Metadata "") :: Token.ListStart :: rest ->
            case List.splitWhen (\t -> t == Token.ListEnd) rest of
                Just ( types, [ Token.ListEnd ] ) ->
                    let
                        possibleMemberTypes =
                            types
                                |> List.map parseType
                                |> Result.combine
                    in
                    case possibleMemberTypes of
                        Err () ->
                            ( () :: errors
                            , ast
                            )

                        Ok members ->
                            ( errors
                            , parseUnionTypeDefinition typeName members ast
                            )

                _ ->
                    ( () :: errors
                    , ast
                    )

        (Token.Metadata "defmulti") :: (Token.Symbol wordName) :: rest ->
            let
                parseMulti meta impl =
                    let
                        isMetaSectionAWhen =
                            List.head >> Maybe.map isWhen >> Maybe.withDefault False

                        ( metaParseErrors, metadata ) =
                            meta
                                |> gather isMeta
                                |> List.filter (isMetaSectionAWhen >> not)
                                |> List.foldl parseMeta ( [], Metadata.default )

                        ( whenParseErrors, whens ) =
                            meta
                                |> gather isMeta
                                |> List.filter isMetaSectionAWhen
                                |> List.foldl parseWhen ( [], [] )

                        parsedImpl =
                            impl
                                |> List.drop 1
                                |> parseAstNodes []
                                |> Result.combine
                    in
                    case ( metaParseErrors, whenParseErrors, parsedImpl ) of
                        ( [], [], Ok wordImpl ) ->
                            ( errors
                            , { ast
                                | words =
                                    Dict.insert wordName
                                        { name = wordName
                                        , metadata = metadata
                                        , implementation = MultiImpl whens wordImpl
                                        }
                                        ast.words
                              }
                            )

                        _ ->
                            ( () :: errors
                            , ast
                            )
            in
            case List.splitWhen (\token -> token == Token.Metadata "") rest of
                Nothing ->
                    parseMulti rest []

                Just ( meta, impl ) ->
                    parseMulti meta impl

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
            case parseWordType values of
                Ok type_ ->
                    ( errors
                    , { metadata | type_ = Just type_ }
                    )

                Err () ->
                    ( () :: errors
                    , metadata
                    )

        _ ->
            ( () :: errors
            , metadata
            )


parseWordType : List Token -> Result () Type.WordType
parseWordType tokens =
    case nestedListSplit Token.QuoteStart Token.QuoteStop Token.TypeSeperator tokens of
        Just ( inputs, outputs ) ->
            let
                possibleInputTypes =
                    parseTypes [] inputs
                        |> Result.combine

                possibleOutputTypes =
                    outputs
                        |> parseTypes []
                        |> Result.combine
            in
            case ( possibleInputTypes, possibleOutputTypes ) of
                ( Ok inputTypes, Ok outputTypes ) ->
                    Ok { input = inputTypes, output = outputTypes }

                _ ->
                    Err ()

        Nothing ->
            Err ()


isWhen : Token -> Bool
isWhen token =
    case token of
        Token.Metadata "when" ->
            True

        _ ->
            False


parseWhen : List Token -> ( List (), List ( TypeMatch, List AstNode ) ) -> ( List (), List ( TypeMatch, List AstNode ) )
parseWhen tokens ( errors, cases ) =
    case tokens of
        (Token.Metadata "when") :: ((Token.Type _) as typeToken) :: impl ->
            let
                parsedImpl =
                    impl
                        |> parseAstNodes []
                        |> Result.combine
            in
            case ( parseTypeMatch typeToken, parsedImpl ) of
                ( Ok type_, Ok wordImpl ) ->
                    ( errors
                    , ( type_, wordImpl ) :: cases
                    )

                _ ->
                    ( () :: errors
                    , cases
                    )

        (Token.Metadata "when") :: (Token.PatternMatchStart typeName) :: remaining ->
            case semanticSplit isPatternMatchStart Token.ParenStop remaining of
                ( pattern, impl ) ->
                    let
                        parsedImpl =
                            impl
                                |> parseAstNodes []
                                |> Result.combine
                    in
                    case ( parsePatternMatch typeName pattern, parsedImpl ) of
                        ( Just ( typeMatch, [] ), Ok wordImpl ) ->
                            ( errors
                            , ( typeMatch, wordImpl ) :: cases
                            )

                        _ ->
                            ( () :: errors
                            , cases
                            )

        _ ->
            ( () :: errors
            , cases
            )


parsePatternMatch : String -> List Token -> Maybe ( TypeMatch, List Token )
parsePatternMatch typeName tokens =
    case parseType (Token.Type typeName) of
        Ok type_ ->
            case semanticSplit isPatternMatchStart Token.ParenStop tokens of
                ( pattern, rem ) ->
                    Just <| ( TypeMatch type_ (collectPatternAttributes pattern []), rem )

        Err _ ->
            Nothing


collectPatternAttributes : List Token -> List ( String, TypeMatchValue ) -> List ( String, TypeMatchValue )
collectPatternAttributes tokens result =
    case tokens of
        [] ->
            List.reverse result

        (Token.Symbol attrValue) :: (Token.Integer val) :: rest ->
            collectPatternAttributes rest (( attrValue, LiteralInt val ) :: result)

        (Token.Symbol attrValue) :: ((Token.Type _) as tokenType) :: rest ->
            case parseType tokenType of
                Ok type_ ->
                    collectPatternAttributes rest (( attrValue, LiteralType type_ ) :: result)

                Err _ ->
                    []

        (Token.Symbol attrValue) :: (Token.PatternMatchStart subName) :: rest ->
            case parsePatternMatch subName rest of
                Just ( typeMatch, remaining ) ->
                    collectPatternAttributes remaining (( attrValue, RecursiveMatch typeMatch ) :: result)

                Nothing ->
                    []

        _ ->
            []


isPatternMatchStart : Token -> Bool
isPatternMatchStart token =
    case token of
        Token.PatternMatchStart _ ->
            True

        _ ->
            False


parseAstNodes : List (Result () AstNode) -> List Token -> List (Result () AstNode)
parseAstNodes result remaining =
    case remaining of
        [] ->
            List.reverse result

        current :: next ->
            case current of
                Token.Integer value ->
                    parseAstNodes (Ok (Integer value) :: result) next

                Token.Symbol value ->
                    parseAstNodes ((Ok <| Word value) :: result) next

                Token.QuoteStart ->
                    case List.splitWhen (\t -> t == Token.QuoteStop) next of
                        Just ( quotImplTokens, newNext ) ->
                            case Result.combine (parseAstNodes [] quotImplTokens) of
                                Ok quotImpl ->
                                    parseAstNodes (Ok (Quotation quotImpl) :: result) (List.drop 1 newNext)

                                _ ->
                                    parseAstNodes (Err () :: result) (List.drop 1 newNext)

                        Nothing ->
                            parseAstNodes (Err () :: result) next

                _ ->
                    parseAstNodes (Err () :: result) next


parseTypeDefinition : String -> List ( String, Type ) -> AST -> AST
parseTypeDefinition typeName members ast =
    let
        typeDef =
            CustomTypeDef typeName members

        metadata =
            Metadata.default
                |> Metadata.withType (List.map Tuple.second members) [ Type.Custom typeName ]

        ctorDef =
            { name = ">" ++ typeName
            , metadata = metadata
            , implementation =
                SoloImpl [ ConstructType typeName ]
            }

        generatedDefs =
            members
                |> List.concatMap setterGetterPair
                |> (::) ctorDef
                |> Dict.fromListBy .name

        setterGetterPair ( memberName, memberType ) =
            [ { name = ">" ++ memberName
              , metadata =
                    Metadata.default
                        |> Metadata.withType [ Type.Custom typeName, memberType ] [ Type.Custom typeName ]
              , implementation =
                    SoloImpl
                        [ SetMember typeName memberName ]
              }
            , { name = memberName ++ ">"
              , metadata =
                    Metadata.default
                        |> Metadata.withType [ Type.Custom typeName ] [ memberType ]
              , implementation =
                    SoloImpl
                        [ GetMember typeName memberName ]
              }
            ]
    in
    { ast
        | types = Dict.insert typeName typeDef ast.types
        , words = Dict.union generatedDefs ast.words
    }


parseType : Token -> Result () Type
parseType token =
    case token of
        Token.Type "Int" ->
            Ok Type.Int

        Token.Type name ->
            Ok <| Type.Custom name

        Token.Symbol genericName ->
            Ok <| Type.Generic genericName

        _ ->
            Err ()


parseTypeMatch : Token -> Result () TypeMatch
parseTypeMatch token =
    case token of
        Token.Type "Int" ->
            Ok <| TypeMatch Type.Int []

        Token.Type name ->
            Ok <| TypeMatch (Type.Custom name) []

        Token.Symbol genericName ->
            Ok <| TypeMatch (Type.Generic genericName) []

        _ ->
            Err ()


parseTypes : List (Result () Type) -> List Token -> List (Result () Type)
parseTypes result remaining =
    case remaining of
        [] ->
            List.reverse result

        current :: next ->
            case current of
                Token.Type "Int" ->
                    parseTypes (Ok Type.Int :: result) next

                Token.Type name ->
                    parseTypes ((Ok <| Type.Custom name) :: result) next

                Token.Symbol genericName ->
                    if String.endsWith "..." genericName then
                        parseTypes ((Ok <| Type.StackRange (String.dropRight 3 genericName)) :: result) next

                    else
                        parseTypes ((Ok <| Type.Generic genericName) :: result) next

                Token.QuoteStart ->
                    case List.splitWhen (\t -> t == Token.QuoteStop) next of
                        Just ( quotTypes, newNext ) ->
                            let
                                possibleQuoteType =
                                    parseWordType quotTypes
                            in
                            case possibleQuoteType of
                                Ok quotType ->
                                    parseTypes (Ok (Type.Quotation quotType) :: result) (List.drop 1 newNext)

                                _ ->
                                    parseTypes (Err () :: result) (List.drop 1 newNext)

                        Nothing ->
                            parseTypes (Err () :: result) next

                _ ->
                    parseTypes (Err () :: result) next


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


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ ->
            name

        UnionTypeDef name _ ->
            name


parseUnionTypeDefinition : String -> List Type -> AST -> AST
parseUnionTypeDefinition typeName members ast =
    let
        typeDef =
            UnionTypeDef typeName members
    in
    { ast | types = Dict.insert typeName typeDef ast.types }


nestedListSplit : Token -> Token -> Token -> List Token -> Maybe ( List Token, List Token )
nestedListSplit newBlockStart newBlockEnd splitter ls =
    nestedListSplitHelper newBlockStart newBlockEnd splitter 0 [] ls


nestedListSplitHelper : Token -> Token -> Token -> Int -> List Token -> List Token -> Maybe ( List Token, List Token )
nestedListSplitHelper newBlockStart newBlockEnd splitter nested before after =
    case after of
        [] ->
            Nothing

        first :: rest ->
            if first == newBlockStart then
                nestedListSplitHelper newBlockStart newBlockEnd splitter (nested + 1) (first :: before) rest

            else if first == newBlockEnd then
                if nested <= 0 then
                    Nothing

                else
                    nestedListSplitHelper newBlockStart newBlockEnd splitter (nested - 1) (first :: before) rest

            else if first == splitter && nested == 0 then
                Just ( List.reverse before, rest )

            else
                nestedListSplitHelper newBlockStart newBlockEnd splitter nested (first :: before) rest


semanticSplit : (Token -> Bool) -> Token -> List Token -> ( List Token, List Token )
semanticSplit newBlockStart newBlockEnd ls =
    semanticSplitHelper newBlockStart newBlockEnd 0 [] ls


semanticSplitHelper : (Token -> Bool) -> Token -> Int -> List Token -> List Token -> ( List Token, List Token )
semanticSplitHelper newBlockStart newBlockEnd nested before after =
    case after of
        [] ->
            ( List.reverse before, [] )

        first :: rest ->
            if newBlockStart first then
                semanticSplitHelper newBlockStart newBlockEnd (nested + 1) (first :: before) rest

            else if first == newBlockEnd then
                if nested <= 0 then
                    ( List.reverse before, rest )

                else
                    semanticSplitHelper newBlockStart newBlockEnd (nested - 1) (first :: before) rest

            else
                semanticSplitHelper newBlockStart newBlockEnd nested (first :: before) rest
