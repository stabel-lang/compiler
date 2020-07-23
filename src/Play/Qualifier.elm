module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type)
import Play.Parser as Parser
import Result.Extra as Result
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
    = SoloImpl (List Node)
    | MultiImpl (List ( TypeMatch, List Node )) (List Node)


type TypeMatch
    = TypeMatch Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type Node
    = Integer Int
    | Word String
    | WordRef String
    | ConstructType String
    | GetMember String String
    | SetMember String String
    | Builtin Builtin


builtinDict : Dict String Node
builtinDict =
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
        |> Dict.fromList
        |> Dict.map (\_ v -> Builtin v)


qualify : Parser.AST -> Result () AST
qualify ast =
    let
        ( typeErrors, qualifiedTypes ) =
            ast.types
                |> Dict.values
                |> List.foldl (qualifyType ast) ( [], Dict.empty )

        ( wordErrors, qualifiedWords ) =
            ast.words
                |> Dict.values
                |> List.foldl (qualifyDefinition ast qualifiedTypes) ( [], Dict.empty )
    in
    case ( typeErrors, wordErrors ) of
        ( [], [] ) ->
            Ok
                { types = qualifiedTypes
                , words = qualifiedWords
                }

        _ ->
            Err ()


qualifyType :
    Parser.AST
    -> Parser.TypeDefinition
    -> ( List (), Dict String TypeDefinition )
    -> ( List (), Dict String TypeDefinition )
qualifyType ast typeDef ( errors, acc ) =
    ( errors
    , case typeDef of
        Parser.CustomTypeDef name generics members ->
            Dict.insert name (CustomTypeDef name generics members) acc

        Parser.UnionTypeDef name generics memberTypes ->
            Dict.insert name (UnionTypeDef name generics memberTypes) acc
    )


qualifyDefinition :
    Parser.AST
    -> Dict String TypeDefinition
    -> Parser.WordDefinition
    -> ( List (), Dict String WordDefinition )
    -> ( List (), Dict String WordDefinition )
qualifyDefinition ast qualifiedTypes unqualifiedWord ( errors, acc ) =
    let
        ( whens, impl ) =
            case unqualifiedWord.implementation of
                Parser.SoloImpl defImpl ->
                    ( [], defImpl )

                Parser.MultiImpl whenImpl defImpl ->
                    ( whenImpl, defImpl )

        ( newWordsAfterWhens, qualifiedWhensResult ) =
            whens
                |> List.foldr (qualifyWhen ast qualifiedTypes unqualifiedWord.name) ( acc, [] )
                |> Tuple.mapSecond Result.combine

        ( newWordsAfterImpl, qualifiedImplementationResult ) =
            List.foldr (qualifyNode ast unqualifiedWord.name) ( newWordsAfterWhens, [] ) impl
                |> Tuple.mapSecond Result.combine
    in
    case ( qualifiedWhensResult, qualifiedImplementationResult ) of
        ( Ok qualifiedWhens, Ok qualifiedImplementation ) ->
            ( errors
            , Dict.insert unqualifiedWord.name
                { name = unqualifiedWord.name
                , metadata = qualifyMetadata unqualifiedWord.name unqualifiedWord.metadata
                , implementation =
                    if List.isEmpty qualifiedWhens then
                        SoloImpl qualifiedImplementation

                    else
                        MultiImpl qualifiedWhens qualifiedImplementation
                }
                newWordsAfterImpl
            )

        _ ->
            ( () :: errors
            , newWordsAfterImpl
            )


qualifyWhen :
    Parser.AST
    -> Dict String TypeDefinition
    -> String
    -> ( Parser.TypeMatch, List Parser.AstNode )
    -> ( Dict String WordDefinition, List (Result () ( TypeMatch, List Node )) )
    -> ( Dict String WordDefinition, List (Result () ( TypeMatch, List Node )) )
qualifyWhen ast qualifiedTypes wordName ( typeMatch, impl ) ( qualifiedWords, result ) =
    let
        ( newWords, qualifiedImplementationResult ) =
            List.foldr (qualifyNode ast wordName) ( qualifiedWords, [] ) impl
                |> Tuple.mapSecond Result.combine

        qualifiedMatchResult =
            qualifyMatch qualifiedTypes typeMatch
    in
    case ( qualifiedImplementationResult, qualifiedMatchResult ) of
        ( Err (), _ ) ->
            ( newWords
            , Err () :: result
            )

        ( _, Err () ) ->
            ( newWords
            , Err () :: result
            )

        ( Ok qualifiedImplementation, Ok qualifiedMatch ) ->
            ( newWords
            , Ok ( qualifiedMatch, qualifiedImplementation ) :: result
            )


qualifyMatch : Dict String TypeDefinition -> Parser.TypeMatch -> Result () TypeMatch
qualifyMatch qualifiedTypes typeMatch =
    case typeMatch of
        Parser.TypeMatch Type.Int [] ->
            Ok <| TypeMatch Type.Int []

        Parser.TypeMatch Type.Int [ ( "value", Parser.LiteralInt val ) ] ->
            Ok <| TypeMatch Type.Int [ ( "value", LiteralInt val ) ]

        Parser.TypeMatch ((Type.Custom name) as type_) patterns ->
            case Dict.get name qualifiedTypes of
                Just (CustomTypeDef _ _ members) ->
                    let
                        memberNames =
                            members
                                |> List.map Tuple.first
                                |> Set.fromList

                        qualifiedPatternsResult =
                            patterns
                                |> List.map (qualifyMatchValue qualifiedTypes memberNames)
                                |> Result.combine
                    in
                    case qualifiedPatternsResult of
                        Ok qualifiedPatterns ->
                            Ok <| TypeMatch type_ qualifiedPatterns

                        Err () ->
                            Err ()

                Just (UnionTypeDef _ _ types) ->
                    if List.isEmpty patterns then
                        Ok <| TypeMatch (Type.Union types) []

                    else
                        Err ()

                Nothing ->
                    Err ()

        _ ->
            Err ()


qualifyMatchValue :
    Dict String TypeDefinition
    -> Set String
    -> ( String, Parser.TypeMatchValue )
    -> Result () ( String, TypeMatchValue )
qualifyMatchValue qualifiedTypes memberNames ( fieldName, matchValue ) =
    if Set.member fieldName memberNames then
        case matchValue of
            Parser.LiteralInt val ->
                Ok <| ( fieldName, LiteralInt val )

            Parser.LiteralType type_ ->
                Ok <| ( fieldName, LiteralType type_ )

            Parser.RecursiveMatch typeMatch ->
                case qualifyMatch qualifiedTypes typeMatch of
                    Err () ->
                        Err ()

                    Ok match ->
                        Ok <| ( fieldName, RecursiveMatch match )

    else
        Err ()


qualifyNode :
    Parser.AST
    -> String
    -> Parser.AstNode
    -> ( Dict String WordDefinition, List (Result () Node) )
    -> ( Dict String WordDefinition, List (Result () Node) )
qualifyNode ast currentDefName node ( qualifiedWords, qualifiedNodes ) =
    case node of
        Parser.Integer value ->
            ( qualifiedWords
            , Ok (Integer value) :: qualifiedNodes
            )

        Parser.Word value ->
            if Dict.member value ast.words then
                ( qualifiedWords
                , Ok (Word value) :: qualifiedNodes
                )

            else
                case Dict.get value builtinDict of
                    Just builtin ->
                        ( qualifiedWords
                        , Ok builtin :: qualifiedNodes
                        )

                    Nothing ->
                        ( qualifiedWords
                        , Err () :: qualifiedNodes
                        )

        Parser.ConstructType typeName ->
            ( qualifiedWords
            , Ok (ConstructType typeName) :: qualifiedNodes
            )

        Parser.SetMember typeName memberName ->
            ( qualifiedWords
            , Ok (SetMember typeName memberName) :: qualifiedNodes
            )

        Parser.GetMember typeName memberName ->
            ( qualifiedWords
            , Ok (GetMember typeName memberName) :: qualifiedNodes
            )

        Parser.Quotation quotImpl ->
            let
                ( newWordsAfterQuot, qualifiedQuotImplResult ) =
                    List.foldr (qualifyNode ast currentDefName) ( qualifiedWords, [] ) quotImpl
                        |> Tuple.mapSecond Result.combine
            in
            case qualifiedQuotImplResult of
                Ok [ Word wordRef ] ->
                    case Dict.get wordRef newWordsAfterQuot of
                        Nothing ->
                            Debug.todo "Cannot happen"

                        Just oldWord ->
                            ( Dict.insert wordRef
                                { oldWord | metadata = Metadata.isQuoted oldWord.metadata }
                                newWordsAfterQuot
                            , Ok (WordRef wordRef) :: qualifiedNodes
                            )

                Ok qualifiedQuotImpl ->
                    let
                        quotBaseName =
                            currentDefName ++ "__" ++ "quot"

                        quotName =
                            quotBaseName ++ String.fromInt (quotId 1)

                        quotId possibleId =
                            case Dict.get (quotBaseName ++ String.fromInt possibleId) newWordsAfterQuot of
                                Just _ ->
                                    quotId (possibleId + 1)

                                Nothing ->
                                    possibleId
                    in
                    ( Dict.insert quotName
                        { name = quotName
                        , metadata =
                            Metadata.default
                                |> Metadata.isQuoted
                        , implementation = SoloImpl qualifiedQuotImpl
                        }
                        newWordsAfterQuot
                    , Ok (WordRef quotName) :: qualifiedNodes
                    )

                Err () ->
                    ( qualifiedWords
                    , Err () :: qualifiedNodes
                    )


qualifyMetadata : String -> Metadata -> Metadata
qualifyMetadata baseName metadata =
    let
        helper { input, output } =
            { input = List.map (qualifyMetadataType baseName) input
            , output = List.map (qualifyMetadataType baseName) output
            }
    in
    { metadata | type_ = Maybe.map helper metadata.type_ }


qualifyMetadataType : String -> Type -> Type
qualifyMetadataType baseName type_ =
    case type_ of
        Type.Generic id ->
            Type.Generic (id ++ "_" ++ baseName)

        _ ->
            type_


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ _ ->
            name

        UnionTypeDef name _ _ ->
            name
