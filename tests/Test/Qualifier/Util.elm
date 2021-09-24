module Test.Qualifier.Util exposing
    ( emptyAst
    , expectModuleOutput
    , expectQualification
    , stripLocations
    )

import Dict
import Expect exposing (Expectation)
import Set
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type exposing (Type)
import Stabel.Parser as Parser
import Stabel.Qualifier as AST
    exposing
        ( AST
        , FunctionDefinition
        , FunctionImplementation(..)
        , Node
        , TypeDefinition
        , TypeMatch(..)
        )


emptyAst : AST
emptyAst =
    { types = Dict.empty
    , functions = Dict.empty
    , referenceableFunctions = Set.empty
    }


expectQualification : String -> Expectation
expectQualification source =
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

                Ok _ ->
                    Expect.pass


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
                        (stripLocations
                            { types = actualAst.types
                            , functions = actualAst.functions
                            , referenceableFunctions = actualAst.referenceableFunctions
                            }
                        )


stripLocations : AST -> AST
stripLocations ast =
    { types = Dict.map (\_ t -> stripTypeLocation t) ast.types
    , functions = Dict.map (\_ d -> stripFunctionLocation d) ast.functions
    , referenceableFunctions = ast.referenceableFunctions
    }


stripTypeLocation : TypeDefinition -> TypeDefinition
stripTypeLocation typeDef =
    { typeDef | sourceLocation = emptyRange }


stripFunctionLocation : FunctionDefinition -> FunctionDefinition
stripFunctionLocation word =
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
            AST.Function emptyRange (stripFunctionLocation val)

        AST.FunctionRef _ val ->
            AST.FunctionRef emptyRange (stripFunctionLocation val)

        AST.Recurse _ ->
            AST.Recurse emptyRange

        AST.Cycle _ data ->
            AST.Cycle emptyRange data

        AST.Builtin _ val ->
            AST.Builtin emptyRange val

        AST.ConstructType t ->
            AST.ConstructType (stripTypeLocation t)

        AST.GetMember td n i t ->
            AST.GetMember (stripTypeLocation td) n i t

        AST.SetMember td n i t ->
            AST.SetMember (stripTypeLocation td) n i t

        AST.ArrayLiteral _ nodes ->
            AST.ArrayLiteral emptyRange nodes


stripMultiWordBranchLocation : ( TypeMatch, List Node ) -> ( TypeMatch, List Node )
stripMultiWordBranchLocation ( typeMatch, nodes ) =
    ( stripTypeMatchLocation typeMatch
    , List.map stripNodeLocation nodes
    )


stripTypeMatchLocation : TypeMatch -> TypeMatch
stripTypeMatchLocation typeMatch =
    case typeMatch of
        TypeMatchInt _ val ->
            TypeMatchInt emptyRange val

        TypeMatchType _ type_ otherConds ->
            TypeMatchType emptyRange type_ <|
                List.map stripTypeMatchCondLocation otherConds


stripTypeMatchCondLocation : ( String, Type, TypeMatch ) -> ( String, Type, TypeMatch )
stripTypeMatchCondLocation ( fieldName, fieldType, match ) =
    ( fieldName, fieldType, stripTypeMatchLocation match )
