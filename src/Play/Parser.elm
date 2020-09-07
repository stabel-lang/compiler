module Play.Parser exposing
    ( AST
    , AstNode(..)
    , Problem(..)
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
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.SourceLocation as SourceLocation exposing (SourceLocation, SourceLocationRange)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Data.TypeSignature as TypeSignature
import Set exposing (Set)


type alias Parser a =
    Parser.Parser () Problem a


type Problem
    = NotInt
    | NotSymbol
    | NotMetadata
    | NotGeneric
    | NotType
    | NoProblem
    | FoundMetadata
    | ExpectedLeftParen
    | ExpectedRightParen
    | ExpectedEnd
    | ExpectedTypeSeperator
    | ExpectedLeftBracket
    | ExpectedRightBracket
    | WordAlreadyDefined String (Maybe SourceLocationRange) (Maybe SourceLocationRange)
    | TypeAlreadyDefined String SourceLocationRange SourceLocationRange
    | UnknownMetadata String


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type TypeDefinition
    = CustomTypeDef SourceLocationRange String (List String) (List ( String, Type ))
    | UnionTypeDef SourceLocationRange String (List String) (List Type)


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatch SourceLocationRange Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type AstNode
    = Integer SourceLocationRange Int
    | Word SourceLocationRange String
    | Quotation SourceLocationRange (List AstNode)
    | ConstructType String
    | GetMember String String
    | SetMember String String


run : String -> Result (List (Parser.DeadEnd () Problem)) AST
run sourceCode =
    Parser.run parser sourceCode



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
                    Parser.problem NotInt
    in
    Parser.variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.empty
        , expecting = NotInt
        }
        |. noiseParser
        |> Parser.andThen helper


sourceLocationParser : Parser SourceLocation
sourceLocationParser =
    Parser.succeed SourceLocation
        |= Parser.getRow
        |= Parser.getCol
        |= Parser.getOffset


symbolParser : Parser String
symbolParser =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = Set.empty
        , expecting = NotSymbol
        }
        |. Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Token ":" NotMetadata)
                |> Parser.andThen (\_ -> Parser.problem FoundMetadata)
            , Parser.succeed identity
            ]
        |. noiseParser
        |> Parser.backtrackable


definitionMetadataParser : Parser String
definitionMetadataParser =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = Set.fromList [ "def", "defmulti", "deftype", "defunion" ]
        , expecting = NotSymbol
        }
        |. Parser.symbol (Token ":" NotMetadata)


genericParser : Parser String
genericParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol (Token "-" NoProblem)
            |> Parser.andThen (\_ -> Parser.problem NotGeneric)
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
        , expecting = NotType
        }
        |. noiseParser


genericOrRangeParser : Parser Type
genericOrRangeParser =
    let
        helper value =
            Parser.oneOf
                [ Parser.succeed (Type.StackRange value)
                    |. Parser.symbol (Token "..." NoProblem)
                    |. noiseParser
                , Parser.succeed (Type.Generic value)
                ]
    in
    Parser.andThen helper genericParser


typeParser : Parser Type
typeParser =
    Parser.oneOf
        [ Parser.succeed Type.Int
            |. Parser.keyword (Token "Int" NoProblem)
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
            |. Parser.keyword (Token "Int" NoProblem)
            |. noiseParser
        , Parser.succeed helper
            |= typeNameParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed Type.Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= Parser.lazy (\_ -> typeRefParser)
            |. Parser.symbol (Token ")" ExpectedRightParen)
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
            |. Parser.lineComment (Token "#" NoProblem)
        , Parser.succeed (Parser.Loop ())
            |. Parser.chompIf (\c -> Set.member c whitespaceChars) NoProblem
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
        |. Parser.end ExpectedEnd


definitionParser : AST -> Parser (Parser.Step AST AST)
definitionParser ast =
    let
        insertWord wordDef =
            case maybeInsertWordProblem wordDef of
                Just problem ->
                    Parser.problem problem

                Nothing ->
                    { ast | words = Dict.insert wordDef.name wordDef ast.words }
                        |> Parser.Loop
                        |> Parser.succeed

        maybeInsertWordProblem wordDef =
            Dict.get wordDef.name ast.words
                |> Maybe.map
                    (\prevDef ->
                        WordAlreadyDefined wordDef.name
                            prevDef.metadata.sourceLocationRange
                            wordDef.metadata.sourceLocationRange
                    )

        insertType typeDef =
            let
                typeName =
                    typeDefinitionName typeDef
            in
            case Dict.get typeName ast.types of
                Just previousDefinition ->
                    Parser.problem <|
                        TypeAlreadyDefined
                            typeName
                            (typeDefinitionLocation previousDefinition)
                            (typeDefinitionLocation typeDef)

                Nothing ->
                    let
                        typeWords =
                            generateDefaultWordsForType typeDef

                        typeWordsProblem =
                            List.filterMap maybeInsertWordProblem typeWords
                                |> List.head
                    in
                    case typeWordsProblem of
                        Just problem ->
                            Parser.problem problem

                        Nothing ->
                            { ast
                                | types = Dict.insert typeName typeDef ast.types
                                , words = Dict.union (Dict.fromListBy .name typeWords) ast.words
                            }
                                |> Parser.Loop
                                |> Parser.succeed
    in
    Parser.oneOf
        [ sourceLocationParser
            |. Parser.keyword (Token "def:" NoProblem)
            |. noiseParser
            |> Parser.andThen wordDefinitionParser
            |> Parser.andThen insertWord
        , sourceLocationParser
            |. Parser.keyword (Token "defmulti:" NoProblem)
            |. noiseParser
            |> Parser.andThen multiWordDefinitionParser
            |> Parser.andThen insertWord
        , sourceLocationParser
            |. Parser.keyword (Token "deftype:" NoProblem)
            |. noiseParser
            |> Parser.andThen typeDefinitionParser
            |> Parser.andThen insertType
        , sourceLocationParser
            |. Parser.keyword (Token "defunion:" NoProblem)
            |. noiseParser
            |> Parser.andThen unionTypeDefinitionParser
            |> Parser.andThen insertType
        , Parser.succeed (Parser.Done ast)
        ]


generateDefaultWordsForType : TypeDefinition -> List WordDefinition
generateDefaultWordsForType typeDef =
    case typeDef of
        UnionTypeDef _ _ _ _ ->
            []

        CustomTypeDef _ typeName binds typeMembers ->
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
                            |> Metadata.withVerifiedType (List.map Tuple.second typeMembers) [ typeOfType ]
                    , implementation =
                        SoloImpl [ ConstructType typeName ]
                    }

                setterGetterPair ( memberName, memberType ) =
                    [ { name = ">" ++ memberName
                      , metadata =
                            Metadata.default
                                |> Metadata.withVerifiedType [ typeOfType, memberType ] [ typeOfType ]
                      , implementation =
                            SoloImpl
                                [ SetMember typeName memberName ]
                      }
                    , { name = memberName ++ ">"
                      , metadata =
                            Metadata.default
                                |> Metadata.withVerifiedType [ typeOfType ] [ memberType ]
                      , implementation =
                            SoloImpl
                                [ GetMember typeName memberName ]
                      }
                    ]
            in
            typeMembers
                |> List.concatMap setterGetterPair
                |> (::) ctorDef


wordDefinitionParser : SourceLocation -> Parser WordDefinition
wordDefinitionParser startLocation =
    let
        joinParseResults name def endLocation =
            { def
                | name = name
                , metadata =
                    Metadata.withSourceLocationRange
                        { start = startLocation
                        , end = endLocation
                        }
                        def.metadata
            }

        emptyDef =
            { name = ""
            , metadata = Metadata.default
            , implementation = SoloImpl []
            }
    in
    Parser.succeed joinParseResults
        |= symbolParser
        |= Parser.loop emptyDef wordMetadataParser
        |= sourceLocationParser


wordMetadataParser : WordDefinition -> Parser (Parser.Step WordDefinition WordDefinition)
wordMetadataParser def =
    let
        metadata =
            def.metadata
    in
    Parser.oneOf
        [ Parser.succeed (\typeSign -> Parser.Loop { def | metadata = { metadata | type_ = TypeSignature.UserProvided typeSign } })
            |. Parser.keyword (Token "type:" NoProblem)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (Parser.Loop { def | metadata = { metadata | isEntryPoint = True } })
            |. Parser.keyword (Token "entry:" NoProblem)
            |. noiseParser
            |. Parser.keyword (Token "true" NoProblem)
            |. noiseParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = SoloImpl impl })
            |. Parser.keyword (Token ":" NoProblem)
            |. noiseParser
            |= implementationParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
        ]


multiWordDefinitionParser : SourceLocation -> Parser WordDefinition
multiWordDefinitionParser startLocation =
    let
        joinParseResults name def endLocation =
            reverseWhens <|
                { def
                    | name = name
                    , metadata =
                        Metadata.withSourceLocationRange
                            { start = startLocation
                            , end = endLocation
                            }
                            def.metadata
                }

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
        |= sourceLocationParser


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
        [ Parser.succeed (\typeSign -> Parser.Loop { def | metadata = { metadata | type_ = TypeSignature.UserProvided typeSign } })
            |. Parser.keyword (Token "type:" NoProblem)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\type_ impl -> Parser.Loop { def | implementation = addWhenImpl ( type_, impl ) })
            |. Parser.keyword (Token "when:" NoProblem)
            |. noiseParser
            |= typeMatchParser
            |= implementationParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = setDefaultImpl impl })
            |. Parser.keyword (Token ":" NoProblem)
            |. noiseParser
            |= implementationParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
        ]


typeDefinitionParser : SourceLocation -> Parser TypeDefinition
typeDefinitionParser startLocation =
    let
        ctor typeName generics members endLocation =
            CustomTypeDef
                (SourceLocationRange startLocation endLocation)
                typeName
                generics
                members
    in
    Parser.succeed ctor
        |= typeNameParser
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] typeMemberParser
        |= sourceLocationParser


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
            |. Parser.symbol (Token ":" NoProblem)
            |. noiseParser
            |= symbolParser
            |= typeRefParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


unionTypeDefinitionParser : SourceLocation -> Parser TypeDefinition
unionTypeDefinitionParser startLocation =
    let
        ctor typeName generics members endLocation =
            UnionTypeDef
                (SourceLocationRange startLocation endLocation)
                typeName
                generics
                members
    in
    Parser.succeed ctor
        |= typeNameParser
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] unionTypeMemberParser
        |= sourceLocationParser


unionTypeMemberParser : List Type -> Parser (Parser.Step (List Type) (List Type))
unionTypeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\type_ -> Parser.Loop (type_ :: types))
            |. Parser.symbol (Token ":" NoProblem)
            |. noiseParser
            |= typeRefParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


typeSignatureParser : Parser WordType
typeSignatureParser =
    Parser.succeed (\input output -> { input = input, output = output })
        |= Parser.loop [] typeLoopParser
        |. Parser.symbol (Token "--" ExpectedTypeSeperator)
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
            |. Parser.symbol (Token "[" ExpectedLeftBracket)
            |. noiseParser
            |= typeSignatureParser
            |. Parser.symbol (Token "]" ExpectedRightBracket)
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse reverseTypes))
        ]


typeMatchParser : Parser TypeMatch
typeMatchParser =
    Parser.oneOf
        [ Parser.succeed (\startLoc type_ conds endLoc -> TypeMatch (SourceLocationRange startLoc endLoc) type_ conds)
            |= sourceLocationParser
            |= typeParser
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol (Token "(" ExpectedLeftParen)
                    |. noiseParser
                    |= Parser.loop [] typeMatchConditionParser
                    |. Parser.symbol (Token ")" ExpectedRightParen)
                , Parser.succeed []
                ]
            |= sourceLocationParser
            |. noiseParser
        , Parser.succeed (\startLoc sym endLoc -> TypeMatch (SourceLocationRange startLoc endLoc) (Type.Generic sym) [])
            |= sourceLocationParser
            |= genericParser
            |= sourceLocationParser
        , Parser.succeed (\startLoc typ endLoc -> TypeMatch (SourceLocationRange startLoc endLoc) typ [])
            |= sourceLocationParser
            |= typeRefParser
            |= sourceLocationParser
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
        handleNewType ((TypeMatch _ type_ conditions) as match) =
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
        , Parser.succeed (\startLoc quotImpl endLoc -> Parser.Loop (Quotation (SourceLocationRange startLoc endLoc) quotImpl :: nodes))
            |= sourceLocationParser
            |. Parser.symbol (Token "[" ExpectedLeftBracket)
            |. noiseParser
            |= implementationParser
            |. Parser.symbol (Token "]" ExpectedRightBracket)
            |= sourceLocationParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse nodes))
        ]


nodeParser : Parser AstNode
nodeParser =
    Parser.oneOf
        [ Parser.succeed (\startLoc value endLoc -> Integer (SourceLocationRange startLoc endLoc) value)
            |= sourceLocationParser
            |= intParser
            |= sourceLocationParser
        , Parser.succeed (\startLoc value endLoc -> Word (SourceLocationRange startLoc endLoc) value)
            |= sourceLocationParser
            |= symbolParser
            |= sourceLocationParser
        ]


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef _ name _ _ ->
            name

        UnionTypeDef _ name _ _ ->
            name


typeDefinitionLocation : TypeDefinition -> SourceLocationRange
typeDefinitionLocation typeDef =
    case typeDef of
        CustomTypeDef range _ _ _ ->
            range

        UnionTypeDef range _ _ _ ->
            range
