module Play.Qualifier exposing (..)

import Dict exposing (Dict)
import Play.Parser as AST
import Set exposing (Set)


type alias Definition =
    { name : String
    , metadata : List ( String, List AST.AstNode )
    , implementation : List Node
    }


type Node
    = Integer Int
    | Word String
    | BuiltinPlus
    | BuiltinMinus
    | BuiltinEqual


builtinDict : Dict String Node
builtinDict =
    Dict.fromList
        [ ( "+", BuiltinPlus )
        , ( "-", BuiltinMinus )
        , ( "=", BuiltinEqual )
        ]


qualify : List AST.Definition -> Result () (List Definition)
qualify ast =
    let
        knownUserDefinitions =
            ast
                |> List.map .name
                |> Set.fromList
    in
    ast
        |> List.map (qualifyDefinition knownUserDefinitions)
        |> listOrError


qualifyDefinition : Set String -> AST.Definition -> Result () Definition
qualifyDefinition knownUserDefinitions definition =
    let
        qualifiedImplementationResult =
            definition.implementation
                |> List.map (qualifyNode knownUserDefinitions)
                |> listOrError
    in
    case qualifiedImplementationResult of
        Err () ->
            Err ()

        Ok qualifiedImplementation ->
            Ok
                { name = definition.name
                , metadata = definition.metadata
                , implementation = qualifiedImplementation
                }


qualifyNode : Set String -> AST.AstNode -> Result () Node
qualifyNode knownUserDefinitions node =
    case node of
        AST.Integer value ->
            Ok (Integer value)

        AST.Word value ->
            if Set.member value knownUserDefinitions then
                Ok (Word value)

            else
                case Dict.get value builtinDict of
                    Just builtin ->
                        Ok builtin

                    Nothing ->
                        Err ()


listOrError : List (Result () a) -> Result () (List a)
listOrError list =
    listOrErrorHelper list []


listOrErrorHelper : List (Result () a) -> List a -> Result () (List a)
listOrErrorHelper potentialTokens tokens =
    case potentialTokens of
        [] ->
            Ok (List.reverse tokens)

        (Err ()) :: _ ->
            Err ()

        (Ok token) :: remaining ->
            listOrErrorHelper remaining (token :: tokens)
