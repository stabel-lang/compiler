module Play.Parser exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Parser exposing ((|.), (|=), Parser)
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type, WordType)
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


run : String -> Result () AST
run sourceCode =
    Parser.run parser sourceCode
        |> Result.mapError (always ())


parser : Parser AST
parser =
    let
        emptyAst =
            { types = Dict.empty
            , words = Dict.empty
            }
    in
    Parser.succeed identity
        |. noiseParser
        |= Parser.loop emptyAst definitionParser
        |. Parser.end


definitionParser : AST -> Parser (Parser.Step AST AST)
definitionParser ast =
    let
        insertWord wordDef =
            Parser.Loop { ast | words = Dict.insert wordDef.name wordDef ast.words }

        insertType typeDef ast_ =
            { ast_ | types = Dict.insert (typeDefinitionName typeDef) typeDef ast_.types }
    in
    Parser.oneOf
        [ Parser.succeed insertWord
            |. Parser.keyword "def:"
            |. noiseParser
            |= wordDefinitionParser
        , Parser.succeed insertWord
            |. Parser.keyword "defmulti:"
            |. noiseParser
            |= multiWordDefinitionParser
        , Parser.succeed (\typeDef -> ast |> insertType typeDef |> generateDefaultWordsForType typeDef |> Parser.Loop)
            |. Parser.keyword "deftype:"
            |. noiseParser
            |= typeDefinitionParser
        , Parser.succeed (\typeDef -> ast |> insertType typeDef |> Parser.Loop)
            |. Parser.keyword "defunion:"
            |. noiseParser
            |= unionTypeDefinitionParser
        , Parser.succeed (Parser.Done ast)
        ]


generateDefaultWordsForType : TypeDefinition -> AST -> AST
generateDefaultWordsForType typeDef ast =
    case typeDef of
        UnionTypeDef _ _ ->
            ast

        CustomTypeDef typeName typeMembers ->
            let
                metadata =
                    Metadata.default
                        |> Metadata.withType (List.map Tuple.second typeMembers) [ Type.Custom typeName ]

                ctorDef =
                    { name = ">" ++ typeName
                    , metadata = metadata
                    , implementation =
                        SoloImpl [ ConstructType typeName ]
                    }

                generatedDefs =
                    typeMembers
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
            { ast | words = Dict.union generatedDefs ast.words }


debugSpaces : String -> Parser ()
debugSpaces tag =
    Parser.chompWhile (\c -> Debug.log tag <| Set.member c whitespaceChars)


wordDefinitionParser : Parser WordDefinition
wordDefinitionParser =
    let
        joinParseResults name def =
            { def | name = name }

        emptyDef =
            { name = ""
            , metadata = Metadata.default
            , implementation = SoloImpl []
            }
    in
    Parser.succeed joinParseResults
        |= symbolParser
        |. noiseParser
        |= Parser.loop emptyDef wordMetadataParser


wordMetadataParser : WordDefinition -> Parser (Parser.Step WordDefinition WordDefinition)
wordMetadataParser def =
    let
        metadata =
            def.metadata
    in
    Parser.oneOf
        [ Parser.succeed (\typeSign -> Parser.Loop { def | metadata = { metadata | type_ = Just typeSign } })
            |. Parser.keyword "type:"
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (Parser.Loop { def | metadata = { metadata | isEntryPoint = True } })
            |. Parser.keyword "entry:"
            |. noiseParser
            |. Parser.keyword "true"
            |. noiseParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = SoloImpl impl })
            |. Parser.keyword ":"
            |. noiseParser
            |= implementationParser
        , Parser.succeed (Parser.Done def)
        ]


multiWordDefinitionParser : Parser WordDefinition
multiWordDefinitionParser =
    let
        joinParseResults name def =
            { def | name = name }

        emptyDef =
            { name = ""
            , metadata = Metadata.default
            , implementation = SoloImpl []
            }
    in
    Parser.succeed joinParseResults
        |= symbolParser
        |. noiseParser
        |= Parser.loop emptyDef multiWordMetadataParser


multiWordMetadataParser : WordDefinition -> Parser (Parser.Step WordDefinition WordDefinition)
multiWordMetadataParser def =
    let
        metadata =
            def.metadata

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
        [ Parser.succeed (\typeSign -> Parser.Loop { def | metadata = { metadata | type_ = Just typeSign } })
            |. Parser.keyword "type:"
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\type_ impl -> Parser.Loop { def | implementation = addWhenImpl ( type_, impl ) })
            |. Parser.keyword "when:"
            |. noiseParser
            |= typeMatchParser
            |. noiseParser
            |= implementationParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = setDefaultImpl impl })
            |. Parser.keyword ":"
            |. noiseParser
            |= implementationParser
        , Parser.succeed (Parser.Done def)
        ]


typeDefinitionParser : Parser TypeDefinition
typeDefinitionParser =
    Parser.succeed CustomTypeDef
        |= typeNameParser
        |. noiseParser
        |= Parser.loop [] typeMemberParser


typeMemberParser : List ( String, Type ) -> Parser (Parser.Step (List ( String, Type )) (List ( String, Type )))
typeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\name type_ -> Parser.Loop (( name, type_ ) :: types))
            |. Parser.symbol ":"
            |. noiseParser
            |= symbolParser
            |. noiseParser
            |= typeParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


unionTypeDefinitionParser : Parser TypeDefinition
unionTypeDefinitionParser =
    Parser.succeed UnionTypeDef
        |= typeNameParser
        |. noiseParser
        |= Parser.loop [] unionTypeMemberParser


unionTypeMemberParser : List Type -> Parser (Parser.Step (List Type) (List Type))
unionTypeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\type_ -> Parser.Loop (type_ :: types))
            |. Parser.symbol ":"
            |. noiseParser
            |= typeParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


{-| The builtin int parser has a bug where it commits when it comes across an 'e'
-}
intParser : Parser Int
intParser =
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


symbolParser : Parser String
symbolParser =
    Parser.backtrackable <|
        Parser.variable
            { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
            , inner = validSymbolChar
            , reserved = Set.empty
            }
            |. Parser.chompIf (\c -> Set.member c whitespaceChars)


metadataParser : Parser String
metadataParser =
    Parser.backtrackable <|
        Parser.variable
            { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
            , inner = validSymbolChar
            , reserved = Set.empty
            }
            |. Parser.chompIf (\c -> c == ':')


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
        ]


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList
        [ ' '
        , '\n'
        , '\u{000D}'
        , '\t'
        ]


typeSignatureParser : Parser WordType
typeSignatureParser =
    Parser.succeed (\input output -> { input = input, output = output })
        |= Parser.loop [] typeLoopParser
        |. Parser.symbol "--"
        |. noiseParser
        |= Parser.loop [] typeLoopParser


typeLoopParser : List Type -> Parser (Parser.Step (List Type) (List Type))
typeLoopParser reverseTypes =
    let
        step type_ =
            Parser.Loop (type_ :: reverseTypes)
    in
    Parser.oneOf
        [ Parser.succeed step
            |= typeParser
            |. noiseParser
        , Parser.succeed step
            |= genericParser
            |. noiseParser
        , Parser.succeed (\wordType -> step (Type.Quotation wordType))
            |. Parser.symbol "["
            |. noiseParser
            |= typeSignatureParser
            |. Parser.symbol "]"
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse reverseTypes))
        ]


typeParser : Parser Type
typeParser =
    let
        helper value =
            if value == "Int" then
                Type.Int

            else
                Type.Custom value
    in
    Parser.map helper typeNameParser


typeNameParser : Parser String
typeNameParser =
    Parser.variable
        { start = Char.isUpper
        , inner = validSymbolChar
        , reserved = Set.empty
        }


typeMatchParser : Parser TypeMatch
typeMatchParser =
    Parser.succeed TypeMatch
        |= typeParser
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol "("
                |. noiseParser
                |= Parser.loop [] typeMatchConditionParser
                |. Parser.symbol ")"
            , Parser.succeed []
            ]
        |. noiseParser


typeMatchConditionParser : List ( String, TypeMatchValue ) -> Parser (Parser.Step (List ( String, TypeMatchValue )) (List ( String, TypeMatchValue )))
typeMatchConditionParser nodes =
    Parser.oneOf
        [ Parser.succeed (\name value -> Parser.Loop (( name, value ) :: nodes))
            |= symbolParser
            |. noiseParser
            |= typeMatchValueParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse nodes))
        ]


typeMatchValueParser : Parser TypeMatchValue
typeMatchValueParser =
    let
        handleNewType ((TypeMatch type_ conditions) as match) =
            case conditions of
                [] ->
                    LiteralType type_

                _ ->
                    RecursiveMatch match
    in
    Parser.oneOf
        [ Parser.succeed LiteralInt
            |= intParser
        , Parser.succeed handleNewType
            |= typeMatchParser
        ]


genericParser : Parser Type
genericParser =
    let
        helper genericName =
            Parser.oneOf
                [ Parser.succeed (Type.StackRange genericName)
                    |. Parser.symbol "..."
                , Parser.succeed (Type.Generic genericName)
                ]
    in
    Parser.variable
        { start = \c -> not (c == '-' || Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = Set.empty
        }
        |> Parser.andThen helper


implementationParser : Parser (List AstNode)
implementationParser =
    Parser.succeed identity
        |= Parser.loop [] implementationParserHelp


implementationParserHelp : List AstNode -> Parser (Parser.Step (List AstNode) (List AstNode))
implementationParserHelp nodes =
    Parser.oneOf
        [ Parser.succeed (\node -> Parser.Loop (node :: nodes))
            |= nodeParser
            |. noiseParser
        , Parser.succeed (\quotImpl -> Parser.Loop (Quotation quotImpl :: nodes))
            |. Parser.symbol "["
            |. noiseParser
            |= implementationParser
            |. Parser.symbol "]"
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse nodes))
        ]


nodeParser : Parser AstNode
nodeParser =
    Parser.oneOf
        [ Parser.succeed Integer
            |= intParser
        , Parser.succeed Word
            |= symbolParser
        ]


noiseParser : Parser ()
noiseParser =
    Parser.loop () noiseParserLoop


noiseParserLoop : () -> Parser (Parser.Step () ())
noiseParserLoop _ =
    Parser.oneOf
        [ Parser.succeed (Parser.Loop ())
            |. Parser.lineComment "#"
        , Parser.succeed (Parser.Loop ())
            |. Parser.chompIf (\c -> Set.member c whitespaceChars)
            |. Parser.chompWhile (\c -> Set.member c whitespaceChars)
        , Parser.succeed (Parser.Done ())
        ]


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ ->
            name

        UnionTypeDef name _ ->
            name
