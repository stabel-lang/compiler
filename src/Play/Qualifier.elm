module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata exposing (Metadata)
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
        qualifiedWhensResult =
            unqualifiedWord.whens
                |> List.map (qualifyWhen ast)
                |> Result.combine

        qualifiedImplementationResult =
            unqualifiedWord.implementation
                |> List.map (qualifyNode ast)
                |> Result.combine
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
                acc
            )

        _ ->
            ( () :: errors
            , acc
            )


qualifyWhen : Parser.AST -> ( Type, List Parser.AstNode ) -> Result () ( Type, List Node )
qualifyWhen ast ( type_, impl ) =
    let
        qualifiedImplementationResult =
            impl
                |> List.map (qualifyNode ast)
                |> Result.combine
    in
    case qualifiedImplementationResult of
        Err () ->
            Err ()

        Ok qualifiedImplementation ->
            Ok ( type_, qualifiedImplementation )


qualifyNode : Parser.AST -> Parser.AstNode -> Result () Node
qualifyNode ast node =
    case node of
        Parser.Integer value ->
            Ok (Integer value)

        Parser.Word value ->
            if Dict.member value ast.words then
                Ok (Word value)

            else
                case Dict.get value builtinDict of
                    Just builtin ->
                        Ok builtin

                    Nothing ->
                        Err ()

        Parser.ConstructType typeName ->
            Ok (ConstructType typeName)

        Parser.SetMember typeName memberName ->
            Ok (SetMember typeName memberName)

        Parser.GetMember typeName memberName ->
            Ok (GetMember typeName memberName)


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
