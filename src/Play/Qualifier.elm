module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata exposing (Metadata)
import Play.Data.Type exposing (Type)
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
    , ( "=", Builtin.Equal )
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
qualifyType ast unqualifiedWord ( errors, acc ) =
    ( errors
    , Dict.insert unqualifiedWord.name unqualifiedWord acc
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
                , metadata = unqualifiedWord.metadata
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
