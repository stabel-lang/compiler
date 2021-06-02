module Test.Qualifier.Util exposing
    ( addFunctionsForStructs
    , expectExternalOutput
    , expectModuleOutput
    , expectOutput
    , stripLocations
    )

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type exposing (Type)
import Play.Parser as Parser
import Play.Qualifier as AST
    exposing
        ( AST
        , Node(..)
        , TypeDefinition
        , TypeMatch(..)
        , TypeMatchValue(..)
        , WordDefinition
        , WordImplementation(..)
        )


emptyAst : AST
emptyAst =
    { types = Dict.empty
    , words = Dict.empty
    }


expectOutput : Parser.AST -> AST -> Expectation
expectOutput parserAst expectedAst =
    let
        result =
            AST.run
                { packageName = ""
                , modulePath = ""
                , ast = parserAst
                , externalModules = Dict.empty
                , inProgressAST = emptyAst
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


expectModuleOutput : Parser.AST -> AST -> Expectation
expectModuleOutput parserAst expectedAst =
    let
        result =
            AST.run
                { packageName = "play/test"
                , modulePath = "some/module"
                , ast = parserAst
                , externalModules = Dict.empty
                , inProgressAST = emptyAst
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


expectExternalOutput : AST -> Parser.AST -> AST -> Expectation
expectExternalOutput inProgressAst parserAst expectedAst =
    let
        config =
            { packageName = ""
            , modulePath = ""
            , ast = parserAst
            , externalModules =
                Dict.fromList
                    [ ( "/mod", "external/package" ) ]
            , inProgressAST = emptyAst
            }

        result =
            AST.run config
    in
    case result of
        Ok _ ->
            Expect.fail "Expected qualification to fail when inProgressAST is missing"

        Err _ ->
            case AST.run { config | inProgressAST = inProgressAst } of
                Err errors ->
                    Expect.fail <| "Did not expect qualification to fail: " ++ Debug.toString errors

                Ok ast ->
                    Expect.equal expectedAst ast


addFunctionsForStructs : AST -> AST
addFunctionsForStructs ast =
    let
        helper _ t wipAst =
            case t of
                AST.CustomTypeDef name _ _ generics members ->
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


stripLocations : AST -> AST
stripLocations ast =
    { types = Dict.map (\_ t -> stripTypeLocation t) ast.types
    , words = Dict.map (\_ d -> stripWordLocation d) ast.words
    }


stripTypeLocation : TypeDefinition -> TypeDefinition
stripTypeLocation typeDef =
    case typeDef of
        AST.CustomTypeDef exposed name _ generics members ->
            AST.CustomTypeDef exposed name emptyRange generics members

        AST.UnionTypeDef exposed name _ generics members ->
            AST.UnionTypeDef exposed name emptyRange generics members


stripWordLocation : WordDefinition -> WordDefinition
stripWordLocation word =
    { word
        | implementation = stripImplementationLocation word.implementation
        , metadata = Metadata.clearSourceLocationRange word.metadata
    }


stripImplementationLocation : WordImplementation -> WordImplementation
stripImplementationLocation impl =
    case impl of
        SoloImpl nodes ->
            SoloImpl (List.map stripNodeLocation nodes)

        MultiImpl conds default ->
            MultiImpl
                (List.map stripMultiWordBranchLocation conds)
                (List.map stripNodeLocation default)


stripNodeLocation : Node -> Node
stripNodeLocation node =
    case node of
        AST.Integer _ val ->
            AST.Integer emptyRange val

        AST.Word _ val ->
            AST.Word emptyRange val

        AST.WordRef _ val ->
            AST.WordRef emptyRange val

        AST.Builtin _ val ->
            AST.Builtin emptyRange val

        _ ->
            node


stripMultiWordBranchLocation : ( TypeMatch, List Node ) -> ( TypeMatch, List Node )
stripMultiWordBranchLocation ( typeMatch, nodes ) =
    ( stripTypeMatchLocation typeMatch
    , List.map stripNodeLocation nodes
    )


stripTypeMatchLocation : TypeMatch -> TypeMatch
stripTypeMatchLocation (TypeMatch _ type_ otherConds) =
    TypeMatch emptyRange type_ <|
        List.map (Tuple.mapSecond stripRecursiveTypeMatchLocation) otherConds


stripRecursiveTypeMatchLocation : TypeMatchValue -> TypeMatchValue
stripRecursiveTypeMatchLocation typeMatchValue =
    case typeMatchValue of
        RecursiveMatch typeMatch ->
            RecursiveMatch (stripTypeMatchLocation typeMatch)

        _ ->
            typeMatchValue
