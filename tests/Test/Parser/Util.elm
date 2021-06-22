module Test.Parser.Util exposing
    ( addFunctionsForStructs
    , compile
    , compileRetainLocations
    , expectAst
    , expectCompiles
    )

import Dict
import Dict.Extra as Dict
import Expect
import Parser.Advanced as Parser
import Stabel.Parser as AST exposing (..)
import Stabel.Parser.Problem exposing (Context, Problem)
import Stabel.Parser.SourceLocation exposing (emptyRange)
import String.Extra as String


compile : String -> Result (List (Parser.DeadEnd Context Problem)) AST
compile str =
    compileRetainLocations str
        |> Result.map stripLocations


compileRetainLocations : String -> Result (List (Parser.DeadEnd Context Problem)) AST
compileRetainLocations str =
    String.unindent str
        |> run ""


stripLocations : AST -> AST
stripLocations ast =
    { sourceReference = ast.sourceReference
    , moduleDefinition = ast.moduleDefinition
    , types = Dict.map (\_ t -> stripTypeLocation t) ast.types
    , functions = Dict.map (\_ d -> stripWordLocation d) ast.functions
    }


stripTypeLocation : TypeDefinition -> TypeDefinition
stripTypeLocation typeDef =
    case typeDef of
        AST.CustomTypeDef _ name generics members ->
            AST.CustomTypeDef emptyRange name generics members

        AST.UnionTypeDef _ name generics members ->
            AST.UnionTypeDef emptyRange name generics members


stripWordLocation : FunctionDefinition -> FunctionDefinition
stripWordLocation word =
    { word
        | implementation = stripImplementationLocation word.implementation
        , sourceLocationRange = Nothing
    }


stripImplementationLocation : FunctionImplementation -> FunctionImplementation
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

        AST.Function _ val ->
            AST.Function emptyRange val

        AST.PackageFunction _ path val ->
            AST.PackageFunction emptyRange path val

        AST.ExternalFunction _ path val ->
            AST.ExternalFunction emptyRange path val

        AST.InlineFunction _ val ->
            AST.InlineFunction emptyRange (List.map stripNodeLocation val)

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


addFunctionsForStructsHelper : String -> List String -> List ( String, AST.PossiblyQualifiedType ) -> AST -> AST
addFunctionsForStructsHelper name generics members ast =
    let
        selfType =
            LocalRef name (List.map Generic generics)

        ctor =
            { name =
                if List.isEmpty members then
                    name

                else
                    ">" ++ name
            , typeSignature =
                Verified
                    { input = List.map Tuple.second members
                    , output = [ selfType ]
                    }
            , sourceLocationRange = Nothing
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation = AST.SoloImpl [ AST.ConstructType name ]
            }

        setters =
            List.map settersHelper members

        settersHelper ( memberName, type_ ) =
            { name = ">" ++ memberName
            , typeSignature =
                Verified
                    { input = [ selfType, type_ ]
                    , output = [ selfType ]
                    }
            , sourceLocationRange = Nothing
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation =
                AST.SoloImpl [ AST.SetMember name memberName ]
            }

        getters =
            List.map gettersHelper members

        gettersHelper ( memberName, type_ ) =
            { name = memberName ++ ">"
            , typeSignature =
                Verified
                    { input = [ selfType ]
                    , output = [ type_ ]
                    }
            , sourceLocationRange = Nothing
            , aliases = Dict.empty
            , imports = Dict.empty
            , implementation =
                AST.SoloImpl [ AST.GetMember name memberName ]
            }

        allFuncs =
            (ctor :: setters)
                ++ getters
                |> Dict.fromListBy .name
    in
    { ast | functions = Dict.union ast.functions allFuncs }


expectCompiles : String -> Expect.Expectation
expectCompiles code =
    case compile code of
        Err err ->
            Expect.fail <| "Did not expect compilation to fail: " ++ Debug.toString err

        Ok _ ->
            Expect.pass


expectAst : String -> AST -> Expect.Expectation
expectAst code expectedAst =
    case compile code of
        Err err ->
            Expect.fail <| "Did not expect compilation to fail: " ++ Debug.toString err

        Ok actualAst ->
            Expect.equal expectedAst actualAst
