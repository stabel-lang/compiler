module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata as Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type)
import Play.Parser as Parser
import Result.Extra as Result


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
    = SoloImpl (List Node)
    | MultiImpl (List ( Type, List Node )) (List Node)


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
                |> List.foldl (qualifyDefinition ast) ( [], Dict.empty )
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
        Parser.CustomTypeDef name members ->
            Dict.insert name (CustomTypeDef name members) acc

        Parser.UnionTypeDef name memberTypes ->
            Dict.insert name (UnionTypeDef name memberTypes) acc
    )


qualifyDefinition :
    Parser.AST
    -> Parser.WordDefinition
    -> ( List (), Dict String WordDefinition )
    -> ( List (), Dict String WordDefinition )
qualifyDefinition ast unqualifiedWord ( errors, acc ) =
    let
        ( newWordsAfterWhens, qualifiedWhensResult ) =
            List.foldr (qualifyWhen ast unqualifiedWord.name) ( acc, [] ) unqualifiedWord.whens
                |> Tuple.mapSecond Result.combine

        ( newWordsAfterImpl, qualifiedImplementationResult ) =
            List.foldr (qualifyNode ast unqualifiedWord.name) ( newWordsAfterWhens, [] ) unqualifiedWord.implementation
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
    -> String
    -> ( Type, List Parser.AstNode )
    -> ( Dict String WordDefinition, List (Result () ( Type, List Node )) )
    -> ( Dict String WordDefinition, List (Result () ( Type, List Node )) )
qualifyWhen ast wordName ( type_, impl ) ( qualifiedWords, result ) =
    let
        ( newWords, qualifiedImplementationResult ) =
            List.foldr (qualifyNode ast wordName) ( qualifiedWords, [] ) impl
                |> Tuple.mapSecond Result.combine
    in
    case qualifiedImplementationResult of
        Err () ->
            ( newWords
            , Err () :: result
            )

        Ok qualifiedImplementation ->
            ( newWords
            , Ok ( type_, qualifiedImplementation ) :: result
            )


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
            case qualifiedQuotImplResult of
                Ok qualifiedQuotImpl ->
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
        CustomTypeDef name _ ->
            name

        UnionTypeDef name _ ->
            name
