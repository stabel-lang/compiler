module Test.Qualifier.Util exposing (addFunctionsForStructs)

import Dict
import Dict.Extra as Dict
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type exposing (Type)
import Play.Qualifier as AST exposing (AST)


addFunctionsForStructs : AST -> AST
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


addFunctionsForStructsHelper : String -> List String -> List ( String, Type ) -> AST -> AST
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
