module Stabel.Parser exposing
    ( AST
    , AstNode(..)
    , FunctionDefinition
    , FunctionImplementation(..)
    , ModuleDefinition(..)
    , ModuleDefinitionRec
    , PossiblyQualifiedType(..)
    , TypeDefinition(..)
    , TypeMatch(..)
    , TypeMatchValue(..)
    , TypeSignature(..)
    , emptyModuleDefinition
    , run
    , typeDefinitionName
    , typeSignatureToMaybe
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Set exposing (Set)
import Stabel.Data.SourceLocation exposing (SourceLocation, SourceLocationRange)
import Stabel.Parser.Problem exposing (..)


type alias Parser a =
    Parser.Parser Context Problem a


type alias AST =
    { moduleDefinition : ModuleDefinition
    , types : Dict String TypeDefinition
    , functions : Dict String FunctionDefinition
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


type alias FunctionDefinition =
    { name : String
    , typeSignature : TypeSignature
    , sourceLocationRange : Maybe SourceLocationRange
    , aliases : Dict String String
    , imports : Dict String (List String)
    , implementation : FunctionImplementation
    }


type alias ParserFunctionType =
    { input : List PossiblyQualifiedType
    , output : List PossiblyQualifiedType
    }


type TypeSignature
    = NotProvided
    | UserProvided ParserFunctionType
    | Verified ParserFunctionType


typeSignatureToMaybe : TypeSignature -> Maybe ParserFunctionType
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
    | FunctionType ParserFunctionType


type FunctionImplementation
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
    | Function SourceLocationRange String
    | PackageFunction SourceLocationRange (List String) String
    | ExternalFunction SourceLocationRange (List String) String
    | InlineFunction SourceLocationRange (List AstNode)
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
                    Parser.problem ExpectedInt
    in
    Parser.variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.empty
        , expecting = ExpectedInt
        }
        |> Parser.andThen helper


sourceLocationParser : Parser SourceLocation
sourceLocationParser =
    Parser.succeed SourceLocation
        |= Parser.getRow
        |= Parser.getCol
        |= Parser.getOffset


symbolParser : Parser String
symbolParser =
    symbolParserHelp
        (\c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars))


symbolImplParser : Parser String
symbolImplParser =
    symbolParserHelp
        (\c -> not (Char.isDigit c || Set.member c invalidSymbolChars))


symbolParserHelp : (Char -> Bool) -> Parser String
symbolParserHelp startPred =
    Parser.variable
        { start = startPred
        , inner = validSymbolChar
        , reserved = Set.empty
        , expecting = ExpectedSymbol
        }
        |. Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Token ":" ExpectedMetadata)
                |> Parser.andThen (\_ -> Parser.problem UnexpectedMetadata)
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


modulePathStringParser : Parser String
modulePathStringParser =
    let
        joiner ( path, sym ) =
            String.join "/" (path ++ [ sym ])
    in
    Parser.oneOf
        [ Parser.succeed identity
            |= symbolImplParser
            |> Parser.andThen (\sym -> Parser.loop [ sym ] modulePathParser)
            |> Parser.map joiner
        , Parser.succeed (\res -> "/" ++ joiner res)
            |= Parser.loop [] modulePathParser
        ]


modulePathParser : List String -> Parser (Parser.Step (List String) ( List String, String ))
modulePathParser symbols =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: symbols))
            |. Parser.symbol (Token "/" ExpectedMetadata)
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


metadataParser : Parser String
metadataParser =
    metadataParserHelp Set.empty


definitionMetadataParser : Parser String
definitionMetadataParser =
    metadataParserHelp <| Set.fromList [ "def", "defmulti", "defstruct", "defunion" ]


metadataParserHelp : Set String -> Parser String
metadataParserHelp reserved =
    Parser.variable
        { start = \c -> not (Char.isDigit c || Char.isUpper c || Set.member c invalidSymbolChars)
        , inner = validSymbolChar
        , reserved = reserved
        , expecting = ExpectedMetadata
        }
        |. Parser.symbol (Token ":" ExpectedMetadata)


textParser : Parser String
textParser =
    Parser.chompWhile (\c -> not <| Set.member c whitespaceChars)
        |> Parser.getChompedString


genericParser : Parser String
genericParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol (Token "-" UnknownError)
            |> Parser.andThen (\_ -> Parser.problem ExpectedGeneric)
        , symbolParser
        ]
        |> Parser.backtrackable


typeNameParser : Parser String
typeNameParser =
    Parser.variable
        { start = Char.isUpper
        , inner = validSymbolChar
        , reserved = Set.empty
        , expecting = ExpectedType
        }


genericOrRangeParser : Parser PossiblyQualifiedType
genericOrRangeParser =
    let
        helper value =
            Parser.oneOf
                [ Parser.succeed (StackRange value)
                    |. Parser.symbol (Token "..." UnknownError)
                , Parser.succeed (Generic value)
                    |. Parser.chompIf (\c -> Set.member c whitespaceChars) ExpectedWhitespace
                ]
    in
    Parser.andThen helper genericParser


typeSignatureRefParser : Parser PossiblyQualifiedType
typeSignatureRefParser =
    Parser.oneOf
        [ Parser.succeed (\name -> LocalRef name [])
            |= typeNameParser
        , Parser.succeed (\( path, name ) -> ExternalRef path name [])
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> InternalRef path name [])
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= typeRefParser
            |. Parser.symbol (Token ")" ExpectedRightParen)
        ]


typeRefParser : Parser PossiblyQualifiedType
typeRefParser =
    Parser.oneOf
        [ Parser.succeed LocalRef
            |= typeNameParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> ExternalRef path name binds)
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed (\( path, name ) binds -> InternalRef path name binds)
            |= Parser.loop [] modularizedTypeRefParser
            |. noiseParser
            |= Parser.loop [] typeOrGenericParser
        , Parser.succeed Generic
            |= genericParser
            |. noiseParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= Parser.lazy (\_ -> typeRefParser)
            |. Parser.symbol (Token ")" ExpectedRightParen)
            |. noiseParser
        ]


typeMatchTypeParser : Parser PossiblyQualifiedType
typeMatchTypeParser =
    Parser.oneOf
        [ Parser.succeed (\name -> LocalRef name [])
            |= typeNameParser
        , Parser.succeed (\( path, name ) -> ExternalRef path name [])
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed (\( path, name ) -> InternalRef path name [])
            |= Parser.loop [] modularizedTypeRefParser
        , Parser.succeed Generic
            |= genericParser
        , Parser.succeed identity
            |. Parser.symbol (Token "(" ExpectedLeftParen)
            |. noiseParser
            |= typeRefParser
            |. Parser.symbol (Token ")" ExpectedRightParen)
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
            |= symbolParser
            |. Parser.symbol (Token "/" ExpectedForwardSlash)
        ]


typeOrGenericParser : List PossiblyQualifiedType -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
typeOrGenericParser types =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (LocalRef name [] :: types))
            |= typeNameParser
            |. noiseParser
        , Parser.succeed (\name -> Parser.Loop (Generic name :: types))
            |= genericParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


noiseParser : Parser ()
noiseParser =
    Parser.loop () noiseParserLoop


noiseParserLoop : () -> Parser (Parser.Step () ())
noiseParserLoop _ =
    Parser.oneOf
        [ Parser.succeed (Parser.Loop ())
            |. Parser.lineComment (Token "#" UnknownError)
        , Parser.succeed (Parser.Loop ())
            |. Parser.chompIf (\c -> Set.member c whitespaceChars) ExpectedWhitespace
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
            , functions = Dict.empty
            }

        checkIfEmpty ast =
            if Dict.isEmpty ast.types && Dict.isEmpty ast.functions then
                Parser.problem ModuleIsEmpty

            else
                Parser.succeed ast
    in
    Parser.inContext TopLevel
        (Parser.succeed identity
            |. noiseParser
            |= Parser.oneOf
                [ Parser.succeed joinParseResults
                    |= moduleDefinitionParser
                    |. noiseParser
                    |= Parser.loop emptyAst definitionParser
                , Parser.succeed (joinParseResults emptyModuleDefinition)
                    |= Parser.loop emptyAst definitionParser
                ]
            |. Parser.end ExpectedEndOfFile
            |> Parser.andThen checkIfEmpty
        )


moduleDefinitionParser : Parser ModuleDefinition
moduleDefinitionParser =
    Parser.inContext ModuleDefinition
        (Parser.succeed identity
            |. Parser.keyword (Token "defmodule:" UnknownError)
            |. noiseParser
            |= Parser.loop emptyModuleDefinitionRec moduleDefinitionMetaParser
            |> Parser.map Defined
        )


moduleDefinitionMetaParser : ModuleDefinitionRec -> Parser (Parser.Step ModuleDefinitionRec ModuleDefinitionRec)
moduleDefinitionMetaParser def =
    Parser.oneOf
        [ Parser.inContext AliasKeyword
            (Parser.succeed (\alias value -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
                |. Parser.keyword (Token "alias:" UnknownError)
                |. noiseParser
                |= symbolParser
                |. noiseParser
                |= modulePathStringParser
                |. noiseParser
            )
        , Parser.inContext ImportKeyword
            (Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
                |. Parser.keyword (Token "import:" UnknownError)
                |. noiseParser
                |= modulePathStringParser
                |. noiseParser
                |= Parser.loop [] symbolImplListParser
                |. noiseParser
            )
        , Parser.inContext ExposingKeyword
            (Parser.succeed (\exposings -> Parser.Loop { def | exposes = exposings })
                |. Parser.keyword (Token "exposing:" UnknownError)
                |. noiseParser
                |= (Parser.loop [] symbolImplListParser |> Parser.map Set.fromList)
                |. noiseParser
            )
        , Parser.succeed UnknownMetadata
            |= metadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
            |. Parser.keyword (Token ":" UnknownError)
            |. noiseParser
        ]


definitionParser : AST -> Parser (Parser.Step AST AST)
definitionParser ast =
    let
        insertFunction funcDef =
            case maybeInsertFunctionProblem funcDef of
                Just problem ->
                    Parser.problem problem

                Nothing ->
                    { ast | functions = Dict.insert funcDef.name funcDef ast.functions }
                        |> Parser.Loop
                        |> Parser.succeed

        maybeInsertFunctionProblem funcDef =
            Dict.get funcDef.name ast.functions
                |> Maybe.map
                    (\prevDef ->
                        FunctionAlreadyDefined funcDef.name
                            prevDef.sourceLocationRange
                            funcDef.sourceLocationRange
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
                        typeFunctions =
                            generateDefaultFunctionsForType typeDef

                        typeFunctionsProblem =
                            List.filterMap maybeInsertFunctionProblem typeFunctions
                                |> List.head
                    in
                    case typeFunctionsProblem of
                        Just problem ->
                            Parser.problem problem

                        Nothing ->
                            { ast
                                | types = Dict.insert typeName typeDef ast.types
                                , functions = Dict.union (Dict.fromListBy .name typeFunctions) ast.functions
                            }
                                |> Parser.Loop
                                |> Parser.succeed
    in
    Parser.oneOf
        [ sourceLocationParser
            |. Parser.keyword (Token "def:" UnknownError)
            |. noiseParser
            |> Parser.andThen functionDefinitionParser
            |> Parser.andThen insertFunction
        , sourceLocationParser
            |. Parser.keyword (Token "defmulti:" UnknownError)
            |. noiseParser
            |> Parser.andThen multiFunctionDefinitionParser
            |> Parser.andThen insertFunction
        , sourceLocationParser
            |. Parser.keyword (Token "defstruct:" UnknownError)
            |. noiseParser
            |> Parser.andThen typeDefinitionParser
            |> Parser.andThen insertType
        , sourceLocationParser
            |. Parser.keyword (Token "defunion:" UnknownError)
            |. noiseParser
            |> Parser.andThen unionTypeDefinitionParser
            |> Parser.andThen insertType
        , Parser.succeed (\start text end -> BadDefinition (SourceLocationRange start end) text)
            |= sourceLocationParser
            |= textParser
            |= sourceLocationParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done ast)
        ]


generateDefaultFunctionsForType : TypeDefinition -> List FunctionDefinition
generateDefaultFunctionsForType typeDef =
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


functionDefinitionParser : SourceLocation -> Parser FunctionDefinition
functionDefinitionParser startLocation =
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
        |. noiseParser
        |= Parser.loop emptyDef functionMetadataParser
        |= sourceLocationParser


functionMetadataParser : FunctionDefinition -> Parser (Parser.Step FunctionDefinition FunctionDefinition)
functionMetadataParser def =
    Parser.oneOf
        [ Parser.succeed (\typeSign -> Parser.Loop { def | typeSignature = UserProvided typeSign })
            |. Parser.keyword (Token "type:" UnknownError)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\alias value -> Parser.Loop { def | aliases = Dict.insert alias value def.aliases })
            |. Parser.keyword (Token "alias:" UnknownError)
            |. noiseParser
            |= symbolParser
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
        , Parser.succeed (\mod vals -> Parser.Loop { def | imports = Dict.insert mod vals def.imports })
            |. Parser.keyword (Token "import:" UnknownError)
            |. noiseParser
            |= modulePathStringParser
            |. noiseParser
            |= Parser.loop [] symbolImplListParser
            |. noiseParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = SoloImpl impl })
            |. Parser.keyword (Token ":" UnknownError)
            |. noiseParser
            |= implementationParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done def)
        ]


multiFunctionDefinitionParser : SourceLocation -> Parser FunctionDefinition
multiFunctionDefinitionParser startLocation =
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
        |. noiseParser
        |= Parser.loop emptyDef multiFunctionMetadataParser
        |= sourceLocationParser


multiFunctionMetadataParser : FunctionDefinition -> Parser (Parser.Step FunctionDefinition FunctionDefinition)
multiFunctionMetadataParser def =
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
            |. Parser.keyword (Token "type:" UnknownError)
            |. noiseParser
            |= typeSignatureParser
        , Parser.succeed (\impl -> Parser.Loop { def | implementation = setDefaultImpl impl })
            |. Parser.keyword (Token "else:" UnknownError)
            |. noiseParser
            |= implementationParser
        , Parser.succeed (\type_ impl -> Parser.Loop { def | implementation = addWhenImpl ( type_, impl ) })
            |. Parser.keyword (Token ":" UnknownError)
            |. noiseParser
            |= typeMatchParser
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
        |. noiseParser
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] typeMemberParser
        |= sourceLocationParser


typeGenericParser : List String -> Parser (Parser.Step (List String) (List String))
typeGenericParser generics =
    Parser.oneOf
        [ Parser.succeed (\name -> Parser.Loop (name :: generics))
            |= genericParser
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse generics))
        ]


typeMemberParser :
    List ( String, PossiblyQualifiedType )
    -> Parser (Parser.Step (List ( String, PossiblyQualifiedType )) (List ( String, PossiblyQualifiedType )))
typeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\name type_ -> Parser.Loop (( name, type_ ) :: types))
            |. Parser.symbol (Token ":" UnknownError)
            |. noiseParser
            |= symbolParser
            |. noiseParser
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
        |. noiseParser
        |= Parser.loop [] typeGenericParser
        |= Parser.loop [] unionTypeMemberParser
        |= sourceLocationParser


unionTypeMemberParser :
    List PossiblyQualifiedType
    -> Parser (Parser.Step (List PossiblyQualifiedType) (List PossiblyQualifiedType))
unionTypeMemberParser types =
    Parser.oneOf
        [ Parser.succeed (\type_ -> Parser.Loop (type_ :: types))
            |. Parser.symbol (Token ":" UnknownError)
            |. noiseParser
            |= typeRefParser
        , Parser.succeed UnknownMetadata
            |= definitionMetadataParser
            |> Parser.andThen Parser.problem
        , Parser.succeed (Parser.Done (List.reverse types))
        ]


typeSignatureParser : Parser ParserFunctionType
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
            |. noiseParser
        , Parser.succeed step
            |= typeSignatureRefParser
            |. noiseParser
        , Parser.succeed (\functionType -> step (FunctionType functionType))
            |. Parser.symbol (Token "[" ExpectedLeftBracket)
            |. noiseParser
            |= typeSignatureParser
            |. Parser.symbol (Token "]" ExpectedRightBracket)
            |. noiseParser
        , Parser.succeed (Parser.Done (List.reverse reverseTypes))
        ]


typeMatchParser : Parser TypeMatch
typeMatchParser =
    Parser.succeed (\startLoc type_ conds endLoc -> TypeMatch (SourceLocationRange startLoc endLoc) type_ conds)
        |= sourceLocationParser
        |= typeMatchTypeParser
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Token "(" ExpectedLeftParen)
                |. noiseParser
                |= Parser.loop [] typeMatchConditionParser
                |. Parser.symbol (Token ")" ExpectedRightParen)
            , Parser.succeed []
            ]
        |= sourceLocationParser


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
            |. noiseParser
        , Parser.succeed (\startLoc quotImpl endLoc -> Parser.Loop (InlineFunction (SourceLocationRange startLoc endLoc) quotImpl :: nodes))
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
            |= qualifiedSymbolImplParser
            |= sourceLocationParser
        ]


qualifiedSymbolImplParser : Parser (SourceLocationRange -> AstNode)
qualifiedSymbolImplParser =
    let
        externalBuilder ( path, reference ) =
            if checkForUpperCaseLetterInPath path then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path

            else if List.length path <= 1 then
                Parser.problem <| InvalidModulePath <| "/" ++ String.join "/" path ++ "/" ++ reference

            else
                Parser.succeed <|
                    \loc ->
                        ExternalFunction loc path reference

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
                            Function loc firstSymbol

                        else
                            PackageFunction loc path reference

        checkForUpperCaseLetterInPath path =
            List.any (String.any Char.isUpper) path
    in
    Parser.oneOf
        [ Parser.succeed internalBuilder
            |= symbolImplParser
            |= Parser.loop [] modulePathParser
            |> Parser.andThen identity
        , Parser.succeed identity
            |= Parser.loop [] modulePathParser
            |> Parser.andThen externalBuilder
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
