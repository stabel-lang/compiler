module Play.Codegen exposing (..)

import Play.Qualifier as AST
import Wasm



-- Bultin function names


stackPushFn : String
stackPushFn =
    "__stack_push"


stackPopFn : String
stackPopFn =
    "__stack_pop"



-- Base module


baseModule : Wasm.Module
baseModule =
    Wasm.initModule
        |> Wasm.withStartFunction
            { name = "__initialize"
            , args = []
            , results = []
            , instructions =
                [ Wasm.NoOp ]
            }
        |> Wasm.withFunction
            { name = stackPushFn
            , args = [ Wasm.Int32 ]
            , results = []
            , instructions =
                [ Wasm.NoOp
                ]
            }
        |> Wasm.withFunction
            { name = stackPopFn
            , args = []
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }


codegen : List AST.Definition -> Result () Wasm.Module
codegen ast =
    let
        funcDefs =
            List.map toWasmFuncDef ast
    in
    List.foldl Wasm.withFunction baseModule funcDefs
        |> Ok


toWasmFuncDef : AST.Definition -> Wasm.FunctionDef
toWasmFuncDef def =
    { name = def.name
    , args = []
    , results = []
    , instructions = List.map nodeToInstruction def.implementation
    }


nodeToInstruction : AST.Node -> Wasm.Instruction
nodeToInstruction node =
    case node of
        AST.Integer value ->
            Wasm.I32_Const value

        AST.Word value ->
            Wasm.Call value

        AST.BuiltinPlus ->
            Wasm.I32_Add

        AST.BuiltinMinus ->
            Wasm.I32_Sub

        AST.BuiltinEqual ->
            Wasm.I32_Eq
