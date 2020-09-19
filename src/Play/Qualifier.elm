module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Data.TypeSignature as TypeSignature
import Play.Parser as Parser
import Play.Qualifier.Problem exposing (Problem(..))
import Result.Extra as Result
import Set exposing (Set)


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type TypeDefinition
    = CustomTypeDef String SourceLocationRange (List String) (List ( String, Type ))
    | UnionTypeDef String SourceLocationRange (List String) (List Type)


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List Node)
    | MultiImpl (List ( TypeMatch, List Node )) (List Node)


type TypeMatch
    = TypeMatch SourceLocationRange Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type Node
    = Integer SourceLocationRange Int
    | Word SourceLocationRange String
    | WordRef SourceLocationRange String
    | ConstructType String
    | GetMember String String
    | SetMember String String
    | Builtin SourceLocationRange Builtin


builtinDict : Dict String Builtin
builtinDict =
    Dict.fromList
        [ ( "+", Builtin.Plus )
        , ( "-", Builtin.Minus )
        , ( "*", Builtin.Multiply )
        , ( "/", Builtin.Divide )
        , ( "=", Builtin.Equal )
        , ( "swap", Builtin.StackSwap )
        , ( "dup", Builtin.StackDuplicate )
        , ( "drop", Builtin.StackDrop )
        , ( "rotate", Builtin.StackRightRotate )
        , ( "-rotate", Builtin.StackLeftRotate )
        , ( "!", Builtin.Apply )
        ]


run : Parser.AST -> Result (List Problem) AST
run ast =
    let
        ( typeErrors, qualifiedTypes ) =
            Dict.foldl (\_ val acc -> qualifyType ast val acc) ( [], Dict.empty ) ast.types
                |> Tuple.mapSecond (\qt -> Dict.map (\_ v -> resolveUnionInTypeDefs qt v) qt)

        ( wordErrors, qualifiedWords ) =
            Dict.foldl (\_ val acc -> qualifyDefinition ast qualifiedTypes val acc) ( [], Dict.empty ) ast.words
    in
    case ( typeErrors, wordErrors ) of
        ( [], [] ) ->
            Ok
                { types = qualifiedTypes
                , words = qualifiedWords
                }

        _ ->
            Err <| typeErrors ++ wordErrors


resolveUnionInTypeDefs : Dict String TypeDefinition -> TypeDefinition -> TypeDefinition
resolveUnionInTypeDefs qt td =
    case td of
        CustomTypeDef name range generics members ->
            CustomTypeDef name range generics (List.map (Tuple.mapSecond (resolveUnion qt)) members)

        UnionTypeDef name range generics memberTypes ->
            UnionTypeDef name range generics (List.map (resolveUnion qt) memberTypes)


resolveUnion : Dict String TypeDefinition -> Type -> Type
resolveUnion typeDefs type_ =
    case type_ of
        Type.Custom typeName ->
            case Dict.get typeName typeDefs of
                Just (UnionTypeDef _ _ _ members) ->
                    Type.Union members

                _ ->
                    type_

        Type.CustomGeneric typeName types ->
            case Dict.get typeName typeDefs of
                Just (UnionTypeDef _ _ generics members) ->
                    let
                        genericsMap =
                            List.map2 Tuple.pair generics types
                                |> Dict.fromList

                        rebindGenerics t =
                            case t of
                                Type.Generic val ->
                                    Dict.get val genericsMap
                                        |> Maybe.withDefault t

                                Type.CustomGeneric cgName cgMembers ->
                                    Type.CustomGeneric cgName <|
                                        List.map rebindGenerics cgMembers

                                _ ->
                                    t
                    in
                    Type.Union (List.map rebindGenerics members)

                _ ->
                    type_

        _ ->
            type_


qualifyType :
    Parser.AST
    -> Parser.TypeDefinition
    -> ( List Problem, Dict String TypeDefinition )
    -> ( List Problem, Dict String TypeDefinition )
qualifyType ast typeDef ( errors, acc ) =
    ( errors
    , case typeDef of
        Parser.CustomTypeDef range name generics members ->
            Dict.insert name (CustomTypeDef name range generics members) acc

        Parser.UnionTypeDef range name generics memberTypes ->
            Dict.insert name (UnionTypeDef name range generics memberTypes) acc
    )


qualifyDefinition :
    Parser.AST
    -> Dict String TypeDefinition
    -> Parser.WordDefinition
    -> ( List Problem, Dict String WordDefinition )
    -> ( List Problem, Dict String WordDefinition )
qualifyDefinition ast qualifiedTypes unqualifiedWord ( errors, acc ) =
    let
        ( whens, impl ) =
            case unqualifiedWord.implementation of
                Parser.SoloImpl defImpl ->
                    ( [], defImpl )

                Parser.MultiImpl whenImpl defImpl ->
                    ( whenImpl, defImpl )

        ( newWordsAfterWhens, qualifiedWhensResult ) =
            List.foldr (qualifyWhen ast qualifiedTypes unqualifiedWord.name) ( acc, [] ) whens
                |> Tuple.mapSecond Result.combine

        ( newWordsAfterImpl, qualifiedImplementationResult ) =
            initQualifyNode unqualifiedWord.name ast newWordsAfterWhens impl

        qualifiedMetadataResult =
            qualifyMetadata qualifiedTypes unqualifiedWord.metadata
    in
    case ( qualifiedWhensResult, qualifiedImplementationResult, qualifiedMetadataResult ) of
        ( Ok qualifiedWhens, Ok qualifiedImplementation, Ok qualifiedMetadata ) ->
            ( errors
            , Dict.insert unqualifiedWord.name
                { name = unqualifiedWord.name
                , metadata = qualifiedMetadata
                , implementation =
                    if List.isEmpty qualifiedWhens then
                        SoloImpl qualifiedImplementation

                    else
                        MultiImpl qualifiedWhens qualifiedImplementation
                }
                newWordsAfterImpl
            )

        ( Err whenError, _, _ ) ->
            ( whenError :: errors
            , newWordsAfterImpl
            )

        ( _, Err implError, _ ) ->
            ( implError :: errors
            , newWordsAfterImpl
            )

        ( _, _, Err metaError ) ->
            ( metaError :: errors
            , newWordsAfterImpl
            )


qualifyMetadata : Dict String TypeDefinition -> Metadata -> Result Problem Metadata
qualifyMetadata qualifiedTypes meta =
    let
        wordRange =
            Maybe.withDefault SourceLocation.emptyRange meta.sourceLocationRange

        typeRefErrors =
            TypeSignature.toMaybe meta.type_
                |> Maybe.map (\ts -> ts.input ++ ts.output)
                |> Maybe.withDefault []
                |> List.filterMap (validateTypeReferences qualifiedTypes wordRange)
    in
    case List.head typeRefErrors of
        Just err ->
            Err err

        Nothing ->
            Ok
                { meta
                    | type_ =
                        TypeSignature.map (resolveUnions qualifiedTypes) meta.type_
                }


validateTypeReferences : Dict String TypeDefinition -> SourceLocationRange -> Type -> Maybe Problem
validateTypeReferences typeDefs wordRange type_ =
    case type_ of
        Type.Custom typeName ->
            case Dict.get typeName typeDefs of
                Just _ ->
                    Nothing

                Nothing ->
                    Just <| UnknownTypeRef wordRange typeName

        Type.CustomGeneric typeName types ->
            case Dict.get typeName typeDefs of
                Just _ ->
                    Nothing

                Nothing ->
                    Just <| UnknownTypeRef wordRange typeName

        _ ->
            Nothing


resolveUnions : Dict String TypeDefinition -> WordType -> WordType
resolveUnions typeDefs wt =
    { input = List.map (resolveUnion typeDefs) wt.input
    , output = List.map (resolveUnion typeDefs) wt.output
    }


qualifyWhen :
    Parser.AST
    -> Dict String TypeDefinition
    -> String
    -> ( Parser.TypeMatch, List Parser.AstNode )
    -> ( Dict String WordDefinition, List (Result Problem ( TypeMatch, List Node )) )
    -> ( Dict String WordDefinition, List (Result Problem ( TypeMatch, List Node )) )
qualifyWhen ast qualifiedTypes wordName ( typeMatch, impl ) ( qualifiedWords, result ) =
    let
        ( newWords, qualifiedImplementationResult ) =
            initQualifyNode wordName ast qualifiedWords impl

        qualifiedMatchResult =
            qualifyMatch qualifiedTypes typeMatch
    in
    case ( qualifiedImplementationResult, qualifiedMatchResult ) of
        ( Err err, _ ) ->
            ( newWords
            , Err err :: result
            )

        ( _, Err err ) ->
            ( newWords
            , Err err :: result
            )

        ( Ok qualifiedImplementation, Ok qualifiedMatch ) ->
            ( newWords
            , Ok ( qualifiedMatch, qualifiedImplementation ) :: result
            )


qualifyMatch : Dict String TypeDefinition -> Parser.TypeMatch -> Result Problem TypeMatch
qualifyMatch qualifiedTypes typeMatch =
    case typeMatch of
        Parser.TypeMatch range Type.Int [] ->
            Ok <| TypeMatch range Type.Int []

        Parser.TypeMatch range Type.Int [ ( "value", Parser.LiteralInt val ) ] ->
            Ok <| TypeMatch range Type.Int [ ( "value", LiteralInt val ) ]

        Parser.TypeMatch range ((Type.Custom name) as type_) patterns ->
            case Dict.get name qualifiedTypes of
                Just (CustomTypeDef _ _ gens members) ->
                    let
                        memberNames =
                            members
                                |> List.map Tuple.first
                                |> Set.fromList

                        qualifiedPatternsResult =
                            patterns
                                |> List.map (qualifyMatchValue qualifiedTypes name memberNames)
                                |> Result.combine

                        actualType =
                            case gens of
                                [] ->
                                    type_

                                _ ->
                                    Type.CustomGeneric name (List.map Type.Generic gens)
                    in
                    case qualifiedPatternsResult of
                        Ok qualifiedPatterns ->
                            Ok <| TypeMatch range actualType qualifiedPatterns

                        Err err ->
                            Err err

                Just (UnionTypeDef _ _ _ types) ->
                    if List.isEmpty patterns then
                        Ok <| TypeMatch range (Type.Union types) []

                    else
                        Err <| UnionTypeMatchWithPatterns range

                Nothing ->
                    Err <| UnknownTypeRef range name

        Parser.TypeMatch range _ _ ->
            Err <| InvalidTypeMatch range


qualifyMatchValue :
    Dict String TypeDefinition
    -> String
    -> Set String
    -> ( String, Parser.TypeMatchValue )
    -> Result Problem ( String, TypeMatchValue )
qualifyMatchValue qualifiedTypes typeName memberNames ( fieldName, matchValue ) =
    if Set.member fieldName memberNames then
        case matchValue of
            Parser.LiteralInt val ->
                Ok <| ( fieldName, LiteralInt val )

            Parser.LiteralType type_ ->
                Ok <| ( fieldName, LiteralType type_ )

            Parser.RecursiveMatch typeMatch ->
                case qualifyMatch qualifiedTypes typeMatch of
                    Err err ->
                        Err err

                    Ok match ->
                        Ok <| ( fieldName, RecursiveMatch match )

    else
        Err <| NoSuchMemberOnType typeName fieldName


initQualifyNode :
    String
    -> Parser.AST
    -> Dict String WordDefinition
    -> List Parser.AstNode
    -> ( Dict String WordDefinition, Result Problem (List Node) )
initQualifyNode currentDefName ast qualifiedWords impl =
    List.foldr (qualifyNode ast currentDefName) ( 1, qualifiedWords, [] ) impl
        |> (\( _, newQualifiedWords, errors ) -> ( newQualifiedWords, Result.combine errors ))


qualifyNode :
    Parser.AST
    -> String
    -> Parser.AstNode
    -> ( Int, Dict String WordDefinition, List (Result Problem Node) )
    -> ( Int, Dict String WordDefinition, List (Result Problem Node) )
qualifyNode ast currentDefName node ( availableQuoteId, qualifiedWords, qualifiedNodes ) =
    case node of
        Parser.Integer loc value ->
            ( availableQuoteId
            , qualifiedWords
            , Ok (Integer loc value) :: qualifiedNodes
            )

        Parser.Word loc value ->
            if Dict.member value ast.words then
                ( availableQuoteId
                , qualifiedWords
                , Ok (Word loc value) :: qualifiedNodes
                )

            else
                case Dict.get value builtinDict of
                    Just builtin ->
                        ( availableQuoteId
                        , qualifiedWords
                        , Ok (Builtin loc builtin) :: qualifiedNodes
                        )

                    Nothing ->
                        ( availableQuoteId
                        , qualifiedWords
                        , Err (UnknownWordRef loc value) :: qualifiedNodes
                        )

        Parser.ConstructType typeName ->
            ( availableQuoteId
            , qualifiedWords
            , Ok (ConstructType typeName) :: qualifiedNodes
            )

        Parser.SetMember typeName memberName ->
            ( availableQuoteId
            , qualifiedWords
            , Ok (SetMember typeName memberName) :: qualifiedNodes
            )

        Parser.GetMember typeName memberName ->
            ( availableQuoteId
            , qualifiedWords
            , Ok (GetMember typeName memberName) :: qualifiedNodes
            )

        Parser.Quotation sourceLocation quotImpl ->
            let
                quoteName =
                    currentDefName ++ "__" ++ "quote" ++ String.fromInt availableQuoteId

                ( newWordsAfterQuot, qualifiedQuotImplResult ) =
                    initQualifyNode quoteName ast qualifiedWords quotImpl
            in
            case qualifiedQuotImplResult of
                Ok [ Word _ wordRef ] ->
                    case Dict.get wordRef newWordsAfterQuot of
                        Nothing ->
                            Debug.todo "Cannot happen"

                        Just oldWord ->
                            ( availableQuoteId
                            , Dict.insert wordRef
                                { oldWord | metadata = Metadata.isQuoted oldWord.metadata }
                                newWordsAfterQuot
                            , Ok (WordRef sourceLocation wordRef) :: qualifiedNodes
                            )

                Ok qualifiedQuotImpl ->
                    ( availableQuoteId + 1
                    , Dict.insert quoteName
                        { name = quoteName
                        , metadata =
                            Metadata.default
                                |> Metadata.isQuoted
                        , implementation = SoloImpl qualifiedQuotImpl
                        }
                        newWordsAfterQuot
                    , Ok (WordRef sourceLocation quoteName) :: qualifiedNodes
                    )

                Err err ->
                    ( availableQuoteId
                    , qualifiedWords
                    , Err err :: qualifiedNodes
                    )


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ _ _ ->
            name

        UnionTypeDef name _ _ _ ->
            name
