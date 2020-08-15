module Play.Parser exposing
    ( AST
    , AstNode(..)
    , TypeDefinition(..)
    , TypeMatch(..)
    , TypeMatchValue(..)
    , WordDefinition
    , WordImplementation(..)
    , run
    , typeDefinitionName
    )

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
    = CustomTypeDef String (List String) (List ( String, Type ))
    | UnionTypeDef String (List String) (List Type)


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



-- ATOMS


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
        , '#'
        ]


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList
        [ ' '
        , '\n'
        , '\u{000D}'
        , '\t'
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
        |. noiseParser
        |> Parser.andThen helper


symbolParser : Parser String
symbolParser =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = Set.empty
        }
        |. Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ":"
                |> Parser.andThen (\_ -> Parser.problem "This is metadata")
            , Parser.succeed identity
            ]
        |. noiseParser
        |> Parser.backtrackable


genericParser : Parser String
genericParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |> Parser.andThen (\_ -> Parser.problem "Generic variables cannot start with -")
        , symbolParser
        ]
        |. noiseParser
        |> Parser.backtrackable


typeNameParser : Parser String
typeNameParser =
    Parser.variable
        { start = Char.isUpper
        , inner = validSymbolChar
        , reserved = Set.empty
        }
        |. noiseParser


genericOrRangeParser : Parser Type
genericOrRangeParser =
    let
        helper value =
            Parser.oneOf
                [ Parser.succeed (Type.StackRange value)
                    |. Parser.symbol "..."
                    |. noiseParser
                , Parser.succeed (Type.Generic value)
                ]
    in
    Parser.andThen helper genericParser


typeParser : Parser Type
typeParser =
    Parser.oneOf
        [ Parser.succeed Type.Int
            |. Parser.keyword "Int"
            |. noiseParser
        , Parser.succeed Type.Custom
            |= typeNameParser
        ]


typeRefParser : Parser Type
typeRefParser =
    let
        helper name binds =
            case binds of
                [] ->
                    Type.Custom name

                _ ->
                    Type.CustomGeneric name binds
    in
    Parser.oneOf
        [ Parser.succeed Type.Int
            |. Parser.keyword "Int"
            |. noiseParser
        , Parser.succeed helper
            |= typeNameParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed Type.Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol "("
            |. noiseParser
            |= Parser.lazy (\_ -> typeRefParser)
            |. Parser.symbol ")"
            |. noiseParser
        ]


typeOrGenericParser : List Type -> Parser (Parser.Step (List Type) (List Type))
typeOrGenericParser types =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (Type.Custom name :: types))
            |= typeNameParser
        , Parser.succeed (\name -> Parser.Loop (Type.Generic name :: types))
            |= genericParser
        , Parser.succeed (Parser.Done (List.reverse types))
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



-- Grammar


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
        UnionTypeDef _ _ _ ->
            ast

        CustomTypeDef typeName binds typeMembers ->
            let
                typeOfType =
                    case binds of
                        [] ->
                            Type.Custom typeName

                        _ ->
                            Type.CustomGeneric typeName (List.map Type.Generic binds)

                ctorDef =
                    { name = ">" ++ typeName
                    , metadata =
                        Metadata.default
                            |> Metadata.withType (List.map Tuple.second typeMembers) [ typeOfType ]
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
                                |> Metadata.withType [ typeOfType, memberType ] [ typeOfType ]
                      , implementation =
                            SoloImpl
                                [ SetMember typeName memberName ]
                      }
                    , { name = memberName ++ ">"
                      , metadata =
                            Metadata.default
                                |> Metadata.withType [ typeOfType ] [ memberType ]
                      , implementation =
                            SoloImpl
                                [ GetMember typeName memberName ]
                      }
                    ]
            in
            { ast | words = Dict.union generatedDefs ast.words }


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
                |> reverseWhens

        emptyDef =
            { name = ""
            , metadata = Metadata.default
            , implementation = SoloImpl []
            }

        reverseWhens def =
            case def.implementation of
                SoloImpl _ ->
                    def

                MultiImpl whens impl ->
                    { def | implementation = MultiImpl (List.reverse whens) impl }
    in
    Parser.succeed joinParseResults
        |= symbolParser
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
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] typeMemberParser


typeGenericParser : List String -> Parser (Parser.Step (List String) (List String))
typeGenericParser generics =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: generics))
            |= genericParser
        , Parser.succeed (Parser.Done (List.reverse generics))
        ]


typeMemberParser : List ( String, Type ) -> Parser (Parser.Step (List ( String, Type )) (List ( String, Type )))
typeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\name type_ -> Parser.Loop (( name, type_ ) :: types))
            |. Parser.symbol ":"
            |. noiseParser
            |= symbolParser
            |= typeRefParser
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


unionTypeDefinitionParser : Parser TypeDefinition
unionTypeDefinitionParser =
    Parser.succeed UnionTypeDef
        |= typeNameParser
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] unionTypeMemberParser


unionTypeMemberParser : List Type -> Parser (Parser.Step (List Type) (List Type))
unionTypeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\type_ -> Parser.Loop (type_ :: types))
            |. Parser.symbol ":"
            |. noiseParser
            |= typeRefParser
        , Parser.succeed (Parser.Done (List.reverse types))
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
        , Parser.succeed step
            |= genericOrRangeParser
        , Parser.succeed step
            |= typeRefParser
        , Parser.succeed (\wordType -> step (Type.Quotation wordType))
            |. Parser.symbol "["
            |. noiseParser
            |= typeSignatureParser
            |. Parser.symbol "]"
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse reverseTypes))
        ]


typeMatchParser : Parser TypeMatch
typeMatchParser =
    Parser.oneOf
        [ Parser.succeed TypeMatch
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
        , Parser.succeed (\sym -> TypeMatch (Type.Generic sym) [])
            |= genericParser
        , Parser.succeed (\typ -> TypeMatch typ [])
            |= typeRefParser
        ]


typeMatchConditionParser : List ( String, TypeMatchValue ) -> Parser (Parser.Step (List ( String, TypeMatchValue )) (List ( String, TypeMatchValue )))
typeMatchConditionParser nodes =
    Parser.oneOf
        [ Parser.succeed (\name value -> Parser.Loop (( name, value ) :: nodes))
            |= symbolParser
            |= typeMatchValueParser
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


implementationParser : Parser (List AstNode)
implementationParser =
    Parser.loop [] implementationParserHelp


implementationParserHelp : List AstNode -> Parser (Parser.Step (List AstNode) (List AstNode))
implementationParserHelp nodes =
    Parser.oneOf
        [ Parser.succeed (\node -> Parser.Loop (node :: nodes))
            |= nodeParser
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


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ _ ->
            name

        UnionTypeDef name _ _ ->
            name
