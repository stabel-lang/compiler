module Play.Parser exposing
    ( AST
    , AstNode(..)
    , ModuleDefinition(..)
    , PossiblyQualifiedType(..)
    , TypeDefinition(..)
    , TypeMatch(..)
    , TypeMatchValue(..)
    , TypeSignature(..)
    , WordDefinition
    , WordImplementation(..)
    , emptyModuleDefinition
    , run
    , typeDefinitionName
    , typeSignatureToMaybe
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Play.Data.SourceLocation exposing (SourceLocation, SourceLocationRange)
import Play.Parser.Problem exposing (..)
import Set exposing (Set)


type alias Parser a =
    Parser.Parser () Problem a


type alias AST =
    { moduleDefinition : ModuleDefinition
    , types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type ModuleDefinition
    = Undefined
    | Defined ModuleDefinitionRec


type alias ModuleDefinitionRec =
    { aliases : Dict String String
    , imports : Dict String (List String)
    , exposes : Set String
    }


type TypeDefinition
    = CustomTypeDef SourceLocationRange String (List String) (List ( String, PossiblyQualifiedType ))
    | UnionTypeDef SourceLocationRange String (List String) (List PossiblyQualifiedType)


type alias WordDefinition =
    { name : String
    , typeSignature : TypeSignature
    , sourceLocationRange : Maybe SourceLocationRange
    , aliases : Dict String String
    , imports : Dict String (List String)
    , implementation : WordImplementation
    }


type alias ParserWordType =
    { input : List PossiblyQualifiedType
    , output : List PossiblyQualifiedType
    }


type TypeSignature
    = NotProvided
    | UserProvided ParserWordType
    | Verified ParserWordType


typeSignatureToMaybe : TypeSignature -> Maybe ParserWordType
typeSignatureToMaybe ts =
    case ts of
        NotProvided ->
            Nothing

        UserProvided wt ->
            Just wt

        Verified wt ->
            Just wt


type PossiblyQualifiedType
    = LocalRef String (List PossiblyQualifiedType)
    | InternalRef (List String) String (List PossiblyQualifiedType)
    | ExternalRef (List String) String (List PossiblyQualifiedType)
    | Generic String
    | StackRange String
    | QuotationType ParserWordType


type WordImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( TypeMatch, List AstNode )) (List AstNode)


type TypeMatch
    = TypeMatch SourceLocationRange PossiblyQualifiedType (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType PossiblyQualifiedType
    | RecursiveMatch TypeMatch


type AstNode
    = Integer SourceLocationRange Int
    | Word SourceLocationRange String
    | PackageWord SourceLocationRange (List String) String
    | ExternalWord SourceLocationRange (List String) String
    | Quotation SourceLocationRange (List AstNode)
    | ConstructType String
    | GetMember String String
    | SetMember String String


run : String -> Result (List Problem) AST
run sourceCode =
    Parser.run parser sourceCode
        |> Result.mapError (List.map .problem)



-- ATOMS


emptyModuleDefinition : ModuleDefinition
emptyModuleDefinition =
    Undefined


emptyModuleDefinitionRec : ModuleDefinitionRec
emptyModuleDefinitionRec =
    { aliases = Dict.empty
    , imports = Dict.empty
    , exposes = Set.empty
    }


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
        , ','
        , '#'
        , '/'
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


symbolParser2 : Parser String
symbolParser2 =
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
        |> Parser.backtrackable


symbolImplParser : Parser String
symbolImplParser =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Set.member c invalidSymbolChars)
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
        |> Parser.backtrackable


symbolImplListParser : List String -> Parser (Parser.Step (List String) (List String))
symbolImplListParser symbols =
    Parser.oneOf
        [ Parser.succeed (\sym -> Parser.Loop (sym :: symbols))
            |= symbolImplParser
            |. noiseParser
        , Parser.succeed (Parser.Done <| List.reverse symbols)
        ]


symbolImplParser2 : Parser (SourceLocationRange -> AstNode)
symbolImplParser2 =
    let
        externalBuilder firstSymbol (( partialPath, reference ) as modulePathResult) =
            let
                path =
                    firstSymbol :: partialPath
            in
            if checkForUpperCaseLetterInPath path then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path

            else if modulePathResult == ( [], "" ) then
                Parser.problem <| InvalidModulePath <| "/" ++ firstSymbol

            else
                Parser.succeed <|
                    \loc ->
                        ExternalWord loc path reference

        internalBuilder firstSymbol (( partialPath, reference ) as modulePathResult) =
            let
                path =
                    firstSymbol :: partialPath
            in
            if checkForUpperCaseLetterInPath path && partialPath /= [] then
                Parser.problem <| InvalidModulePath <| String.join "/" path

            else
                Parser.succeed <|
                    \loc ->
                        if modulePathResult == ( [], "" ) then
                            Word loc firstSymbol

                        else
                            PackageWord loc path reference

        checkForUpperCaseLetterInPath path =
            List.any (String.any Char.isUpper) path
    in
    Parser.oneOf
        [ Parser.succeed externalBuilder
            |. Parser.symbol (Token "/" NotMetadata)
            |= symbolImplParser
            |= Parser.loop [] modulePathParser
        , Parser.succeed internalBuilder
            |= symbolImplParser
            |= Parser.oneOf
                [ Parser.loop [] modulePathParser
                , Parser.succeed ( [], "" )
                ]
        ]
        |. noiseParser
        |> Parser.andThen identity


modulePathStringParser : Parser String
modulePathStringParser =
    Parser.oneOf
        [ Parser.succeed identity
            |= symbolImplParser
            |> Parser.andThen (\sym -> Parser.loop sym moduleRefParser)
        , Parser.succeed identity
            |= Parser.loop "" moduleRefParser
        ]


moduleRefParser : String -> Parser (Parser.Step String String)
moduleRefParser path =
    Parser.oneOf
        [ Parser.succeed (\part -> Parser.Loop (path ++ "/" ++ part))
            |. Parser.symbol (Token "/" NotMetadata)
            |= symbolImplParser
        , Parser.succeed (Parser.Done path)
        ]


modulePathParser : List String -> Parser (Parser.Step (List String) ( List String, String ))
modulePathParser symbols =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: symbols))
            |. Parser.symbol (Token "/" NotMetadata)
            |= symbolImplParser
        , Parser.succeed (Parser.Done (modulePathFinalizer symbols))
        ]


modulePathFinalizer : List String -> ( List String, String )
modulePathFinalizer symbols =
    case symbols of
        [] ->
            ( [], "" )

        [ only ] ->
            ( [], only )

        first :: rest ->
            ( List.reverse rest, first )


definitionMetadataParser : Parser String
definitionMetadataParser =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = Set.fromList [ "def", "defmulti", "defstruct", "defunion" ]
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


genericOrRangeParser : Parser PossiblyQualifiedType
genericOrRangeParser =
    let
        helper value =
            Parser.oneOf
                [ Parser.succeed (StackRange value)
                    |. Parser.symbol (Token "..." NoProblem)
                    |. noiseParser
                , Parser.succeed (Generic value)
                    |. Parser.chompIf (\c -> Set.member c whitespaceChars) NoProblem
                ]
    in
    Parser.andThen helper genericParser


typeParser : Parser PossiblyQualifiedType
typeParser =
    Parser.succeed (\name -> LocalRef name [])
        |= typeNameParser


typeRefParser : Parser PossiblyQualifiedType
typeRefParser =
    Parser.oneOf
        [ Parser.succeed LocalRef
            |= typeNameParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> ExternalRef path name binds)
            |. Parser.symbol (Token "/" NoProblem)
            |= Parser.loop [] modularizedTypeRefParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> InternalRef path name binds)
            |= Parser.loop [] modularizedTypeRefParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= Parser.lazy (\_ -> typeRefParser)
            |. Parser.symbol (Token ")" ExpectedRightParen)
            |. noiseParser
        ]


modularizedTypeRefParser :
    List String
    -> Parser (Parser.Step (List String) ( List String, String ))
modularizedTypeRefParser reversedPath =
    let
        onType type_ =
            Parser.Done
                ( List.reverse reversedPath
                , type_
                )

        addToPath pathPiece =
            Parser.Loop (pathPiece :: reversedPath)
    in
    Parser.oneOf
        [ Parser.succeed onType
            |= typeNameParser
        , Parser.succeed addToPath
            |= symbolParser2
            |. Parser.symbol (Token "/" NoProblem)
        ]


typeOrGenericParser : List PossiblyQualifiedType -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
typeOrGenericParser types =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (LocalRef name [] :: types))
            |= typeNameParser
        , Parser.succeed (\name -> Parser.Loop (Generic name :: types))
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
        joinParseResults modDef ast =
            { ast | moduleDefinition = modDef }

        emptyAst =
            { moduleDefinition = emptyModuleDefinition
            , types = Dict.empty
            , words = Dict.empty
            }
    in
    Parser.succeed identity
        |. noiseParser
        |= Parser.oneOf
            [ Parser.succeed joinParseResults
                |= moduleDefinitionParser
                |. noiseParser
                |= Parser.loop emptyAst definitionParser
            , Parser.succeed (joinParseResults emptyModuleDefinition)
                |= Parser.loop emptyAst definitionParser
            ]
        |. Parser.end ExpectedEnd


moduleDefinitionParser : Parser ModuleDefinition
moduleDefinitionParser =
    Parser.succeed identity
        |. Parser.keyword (Token "defmodule:" NoProblem)
        |. noiseParser
        |= Parser.map Defined (Parser.loop emptyModuleDefinitionRec moduleDefinitionMetaParser)


moduleDefinitionMetaParser : ModuleDefinitionRec -> Parser (Parser.Step ModuleDefinitionRec ModuleDefinitionRec)
moduleDefinitionMetaParser def =
    Parser.oneOf
        [ Parser.succeed (\alias value -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
            |. Parser.keyword (Token "alias:" NoProblem)
            |. noiseParser
            |= symbolParser
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
        , Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
            |. Parser.keyword (Token "import:" NoProblem)
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
            |= Parser.loop [] symbolImplListParser
            |. noiseParser
        , Parser.succeed (\exposings -> Parser.Loop { def | exposes = exposings })
            |. Parser.keyword (Token "exposing:" NoProblem)
            |. noiseParser
            |= (Parser.loop [] symbolImplListParser |> Parser.map Set.fromList)
            |. noiseParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
            |. Parser.keyword (Token ":" NoProblem)
            |. noiseParser
        ]


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
                            prevDef.sourceLocationRange
                            wordDef.sourceLocationRange
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
            |. Parser.keyword (Token "defstruct:" NoProblem)
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
                    LocalRef typeName (List.map Generic binds)

                ctorDef =
                    { name =
                        if List.isEmpty typeMembers then
                            typeName

                        else
                            ">" ++ typeName
                    , typeSignature =
                        Verified
                            { input = List.map Tuple.second typeMembers
                            , output = [ typeOfType ]
                            }
                    , sourceLocationRange = Nothing
                    , aliases = Dict.empty
                    , imports = Dict.empty
                    , implementation =
                        SoloImpl [ ConstructType typeName ]
                    }

                setterGetterPair ( memberName, memberType ) =
                    [ { name = ">" ++ memberName
                      , typeSignature =
                            Verified
                                { input = [ typeOfType, memberType ]
                                , output = [ typeOfType ]
                                }
                      , sourceLocationRange = Nothing
                      , aliases = Dict.empty
                      , imports = Dict.empty
                      , implementation =
                            SoloImpl
                                [ SetMember typeName memberName ]
                      }
                    , { name = memberName ++ ">"
                      , typeSignature =
                            Verified
                                { input = [ typeOfType ]
                                , output = [ memberType ]
                                }
                      , sourceLocationRange = Nothing
                      , aliases = Dict.empty
                      , imports = Dict.empty
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
                , sourceLocationRange =
                    Just
                        { start = startLocation
                        , end = endLocation
                        }
            }

        emptyDef =
            { name = ""
            , typeSignature = NotProvided
            , sourceLocationRange = Nothing
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation = SoloImpl []
            }
    in
    Parser.succeed joinParseResults
        |= symbolParser
        |= Parser.loop emptyDef wordMetadataParser
        |= sourceLocationParser


wordMetadataParser : WordDefinition -> Parser (Parser.Step WordDefinition WordDefinition)
wordMetadataParser def =
    Parser.oneOf
        [ Parser.succeed (\typeSign -> Parser.Loop { def | typeSignature = UserProvided typeSign })
            |. Parser.keyword (Token "type:" NoProblem)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\alias value -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
            |. Parser.keyword (Token "alias:" NoProblem)
            |. noiseParser
            |= symbolParser
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
        , Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
            |. Parser.keyword (Token "import:" NoProblem)
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
            |= Parser.loop [] symbolImplListParser
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
                    , sourceLocationRange =
                        Just
                            { start = startLocation
                            , end = endLocation
                            }
                }

        emptyDef =
            { name = ""
            , typeSignature = NotProvided
            , sourceLocationRange = Nothing
            , aliases = Dict.empty
            , imports = Dict.empty
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
        [ Parser.succeed (\typeSign -> Parser.Loop { def | typeSignature = UserProvided typeSign })
            |. Parser.keyword (Token "type:" NoProblem)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = setDefaultImpl impl })
            |. Parser.keyword (Token "else:" NoProblem)
            |. noiseParser
            |= implementationParser
        , Parser.succeed (\type_ impl -> Parser.Loop { def | implementation = addWhenImpl ( type_, impl ) })
            |. Parser.keyword (Token ":" NoProblem)
            |. noiseParser
            |= typeMatchParser
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


typeMemberParser :
    List ( String, PossiblyQualifiedType )
    -> Parser (Parser.Step (List ( String, PossiblyQualifiedType )) (List ( String, PossiblyQualifiedType )))
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


unionTypeMemberParser :
    List PossiblyQualifiedType
    -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
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


typeSignatureParser : Parser ParserWordType
typeSignatureParser =
    Parser.succeed (\input output -> { input = input, output = output })
        |= Parser.loop [] typeLoopParser
        |. Parser.symbol (Token "--" ExpectedTypeSeperator)
        |. noiseParser
        |= Parser.loop [] typeLoopParser


typeLoopParser :
    List PossiblyQualifiedType
    -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
typeLoopParser reverseTypes =
    let
        step type_ =
            Parser.Loop (type_ :: reverseTypes)
    in
    Parser.oneOf
        [ Parser.succeed step
            |= genericOrRangeParser
        , Parser.succeed step
            |= typeRefParser
        , Parser.succeed (\wordType -> step (QuotationType wordType))
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
        , Parser.succeed (\startLoc sym endLoc -> TypeMatch (SourceLocationRange startLoc endLoc) (Generic sym) [])
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
        , Parser.succeed (\startLoc builder endLoc -> builder (SourceLocationRange startLoc endLoc))
            |= sourceLocationParser
            |= symbolImplParser2
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
