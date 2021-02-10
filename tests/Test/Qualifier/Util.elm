module Test.Qualifier.Util exposing
    ( addFunctionsForStructs
    , expectModuleOutput
    , expectOutput
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type exposing (Type)
import Play.Parser as Parser
import Play.Qualifier as AST exposing (AST, TypeDefinition, WordDefinition)
import Play.Qualifier.Problem exposing (Problem)
import Set


type alias FullyLoadedAST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


expectOutput : Parser.AST -> FullyLoadedAST -> Expectation
expectOutput parserAst expectedAst =
    let
        result =
            AST.run
                { packageName = ""
                , modulePath = ""
                , ast = parserAst
                }
    in
    case result of
        Err errors ->
            Expect.fail <| "Did not expect qualification to fail. Errors: " ++ Debug.toString errors

        Ok actualAst ->
            Expect.equal expectedAst
                { types = actualAst.types
                , words = actualAst.words
                }


expectModuleOutput : Parser.AST -> FullyLoadedAST -> Expectation
expectModuleOutput parserAst expectedAst =
    let
        result =
            AST.run
                { packageName = "play/test"
                , modulePath = "some/module"
                , ast = parserAst
                }
    in
    case result of
        Err errors ->
            Expect.fail <| "Did not expect qualification to fail. Errors: " ++ Debug.toString errors

        Ok actualAst ->
            Expect.equal expectedAst
                { types = actualAst.types
                , words = actualAst.words
                }


addFunctionsForStructs : FullyLoadedAST -> FullyLoadedAST
addFunctionsForStructs ast =
    let
        helper _ t wipAst =
            case t of
                AST.CustomTypeDef name _ generics members ->
                    addFunctionsForStructsHelper name generics members wipAst

                _ ->
                    wipAst
    in
    Dict.foldl helper ast ast.types


addFunctionsForStructsHelper : String -> List String -> List ( String, Type ) -> FullyLoadedAST -> FullyLoadedAST
addFunctionsForStructsHelper name generics members ast =
    let
        selfType =
            if List.isEmpty generics then
                Type.Custom name

            else
                Type.CustomGeneric name (List.map Type.Generic generics)

        ctor =
            { name =
                if List.isEmpty members then
                    name

                else
                    ">" ++ name
            , metadata =
                Metadata.default
                    |> Metadata.withVerifiedType (List.map Tuple.second members) [ selfType ]
            , implementation = AST.SoloImpl [ AST.ConstructType name ]
            }

        setters =
            List.map settersHelper members

        settersHelper ( memberName, type_ ) =
            { name = ">" ++ memberName
            , metadata =
                Metadata.default
                    |> Metadata.withVerifiedType
                        [ selfType, type_ ]
                        [ selfType ]
            , implementation =
                AST.SoloImpl [ AST.SetMember name memberName ]
            }

        getters =
            List.map gettersHelper members

        gettersHelper ( memberName, type_ ) =
            { name = memberName ++ ">"
            , metadata =
                Metadata.default
                    |> Metadata.withVerifiedType
                        [ selfType ]
                        [ type_ ]
            , implementation =
                AST.SoloImpl [ AST.GetMember name memberName ]
            }

        allFuncs =
            (ctor :: setters)
                ++ getters
                |> Dict.fromListBy .name
    in
    { ast | words = Dict.union ast.words allFuncs }
