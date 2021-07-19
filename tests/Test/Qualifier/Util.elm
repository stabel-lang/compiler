module Test.Qualifier.Util exposing
    ( expectModuleOutput
    , stripLocations
    )

import Dict
import Dict.Extra as Dict
import Expect exposing (Expectation)
import Set
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Parser as Parser
import Stabel.Qualifier as AST
    exposing
        ( AST
        , FunctionDefinition
        , FunctionImplementation(..)
        , Node(..)
        , TypeDefinition
        , TypeMatch(..)
        , TypeMatchValue(..)
        )


emptyAst : AST
emptyAst =
    { types = Dict.empty
    , functions = Dict.empty
    , referenceableFunctions = Set.empty
    }


expectModuleOutput : String -> AST -> Expectation
expectModuleOutput source expectedAst =
    case Parser.run "test" source of
        Err errors ->
            Expect.fail <| "Parser error: " ++ Debug.toString errors

        Ok parserAst ->
            let
                result =
                    AST.run
                        { packageName = "stabel/test"
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
                        , functions = actualAst.functions
                        , referenceableFunctions = actualAst.referenceableFunctions
                        }


stripLocations : AST -> AST
stripLocations ast =
    { types = Dict.map (\_ t -> stripTypeLocation t) ast.types
    , functions = Dict.map (\_ d -> stripWordLocation d) ast.functions
    , referenceableFunctions = ast.referenceableFunctions
    }


stripTypeLocation : TypeDefinition -> TypeDefinition
stripTypeLocation typeDef =
    { typeDef | sourceLocation = emptyRange }


stripWordLocation : FunctionDefinition -> FunctionDefinition
stripWordLocation word =
    { word
        | implementation = stripImplementationLocation word.implementation
        , sourceLocation = Nothing
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


stripNodeLocation : Node -> Node
stripNodeLocation node =
    case node of
        AST.Integer _ val ->
            AST.Integer emptyRange val

        AST.Function _ val ->
            AST.Function emptyRange val

        AST.FunctionRef _ val ->
            AST.FunctionRef emptyRange val

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
