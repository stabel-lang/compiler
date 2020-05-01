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


type alias TypeDefinition =
    { name : String
    , members : List ( String, Type )
    }


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : List Node
    }


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
            Dict.insert name { name = name, members = members } acc

        _ ->
            acc
    )


qualifyDefinition :
    Parser.AST
    -> Parser.WordDefinition
    -> ( List (), Dict String WordDefinition )
    -> ( List (), Dict String WordDefinition )
qualifyDefinition ast unqualifiedWord ( errors, acc ) =
    let
        qualifiedImplementationResult =
            unqualifiedWord.implementation
                |> List.map (qualifyNode ast)
                |> Result.combine
    in
    case qualifiedImplementationResult of
        Err () ->
            ( () :: errors
            , acc
            )

        Ok qualifiedImplementation ->
            ( errors
            , Dict.insert unqualifiedWord.name
                { name = unqualifiedWord.name
                , metadata = qualifyMetadata unqualifiedWord.name unqualifiedWord.metadata
                , implementation = qualifiedImplementation
                }
                acc
            )


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
