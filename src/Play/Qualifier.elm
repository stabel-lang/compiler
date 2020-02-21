module Play.Qualifier exposing
    ( Builtin(..)
    , Module
    , Node(..)
    , TopLevel(..)
    , qualify
    )

import Dict exposing (Dict)
import Play.Parser as Parser


type alias Module =
    { ast : Dict String TopLevel }


type TopLevel
    = Def String (List String) (List Node)


type Node
    = BuiltinRef Builtin
    | LocalRef String
    | ModuleRef String
    | ConstInt Int


type Builtin
    = IntAdd
    | IntSub


qualify : Dict String Module -> Parser.Module -> Result String Module
qualify existingModules moduleAst =
    let
        qualifiedAstResult =
            Dict.foldl qualifyHelp (Ok Dict.empty) moduleAst.ast

        qualifyHelp name node lastResult =
            case lastResult of
                Err _ ->
                    lastResult

                Ok acc ->
                    case qualifyNode existingModules moduleAst.ast name node of
                        Ok qualifiedNode ->
                            Ok <| Dict.insert name qualifiedNode acc

                        Err error ->
                            Err error
    in
    case qualifiedAstResult of
        Err err ->
            Err err

        Ok qualifiedAst ->
            Ok { ast = qualifiedAst }


qualifyNode : Dict String Module -> Dict String Parser.TopLevel -> String -> Parser.TopLevel -> Result String TopLevel
qualifyNode existingModules currentModule name def =
    case def of
        Parser.Def _ args nodes ->
            qualifyDef
                { otherModules = existingModules
                , currentModule = currentModule
                , name = name
                , args = args
                }
                nodes
                []


type alias Context =
    { otherModules : Dict String Module
    , currentModule : Dict String Parser.TopLevel
    , name : String
    , args : List String
    }


qualifyDef : Context -> List Parser.Node -> List Node -> Result String TopLevel
qualifyDef context nodes acc =
    case nodes of
        [] ->
            Ok <| Def context.name context.args (List.reverse acc)

        first :: rest ->
            case first of
                Parser.Integer value ->
                    qualifyDef context rest (ConstInt value :: acc)

                Parser.Symbol "builtin_plus" ->
                    qualifyDef context rest (BuiltinRef IntAdd :: acc)

                Parser.Symbol "builtin_sub" ->
                    qualifyDef context rest (BuiltinRef IntSub :: acc)

                Parser.Symbol ref ->
                    if List.member ref context.args then
                        qualifyDef context rest (LocalRef ref :: acc)

                    else if Dict.get ref context.currentModule /= Nothing then
                        qualifyDef context rest (ModuleRef ref :: acc)

                    else
                        Err <| "Could not find reference '" ++ ref ++ "'"
