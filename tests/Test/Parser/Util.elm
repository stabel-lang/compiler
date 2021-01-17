module Test.Parser.Util exposing
    ( addFunctionsForStructs
    , compile
    , compileRetainLocations
    )

import Dict
import Dict.Extra as Dict
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (emptyRange)
import Play.Data.Type as Type exposing (Type)
import Play.Parser as AST exposing (..)
import Play.Parser.Problem exposing (Problem)
import String.Extra as String


compile : String -> Result (List Problem) AST
compile str =
    compileRetainLocations str
        |> Result.map stripLocations


compileRetainLocations : String -> Result (List Problem) AST
compileRetainLocations str =
    String.unindent str
        |> run


stripLocations : AST -> AST
stripLocations ast =
    { types = Dict.map (\_ t -> stripTypeLocation t) ast.types
    , words = Dict.map (\_ d -> stripWordLocation d) ast.words
    }


stripTypeLocation : TypeDefinition -> TypeDefinition
stripTypeLocation typeDef =
    case typeDef of
        AST.CustomTypeDef _ name generics members ->
            AST.CustomTypeDef emptyRange name generics members

        AST.UnionTypeDef _ name generics members ->
            AST.UnionTypeDef emptyRange name generics members


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


stripNodeLocation : AstNode -> AstNode
stripNodeLocation node =
    case node of
        AST.Integer _ val ->
            AST.Integer emptyRange val

        AST.Word _ val ->
            AST.Word emptyRange val

        AST.Quotation _ val ->
            AST.Quotation emptyRange (List.map stripNodeLocation val)

        _ ->
            node


stripMultiWordBranchLocation : ( TypeMatch, List AstNode ) -> ( TypeMatch, List AstNode )
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


addFunctionsForStructs : AST -> AST
addFunctionsForStructs ast =
    let
        helper _ t wipAst =
            case t of
                AST.CustomTypeDef _ name generics members ->
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
            { name = ">" ++ name
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
