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


pushIntfn : String
pushIntfn =
    "__push_i32"


addIntFn : String
addIntFn =
    "__add_i32"


subIntFn : String
subIntFn =
    "__sub_i32"


eqIntFn : String
eqIntFn =
    "__eq_i32"



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
        |> Wasm.withFunction
            { name = pushIntfn
            , args = [ Wasm.Int32, Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = addIntFn
            , args = [ Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = subIntFn
            , args = [ Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = eqIntFn
            , args = [ Wasm.Int32 ]
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
            Wasm.Batch
                [ Wasm.I32_Const value
                , Wasm.Call pushIntfn
                ]

        AST.Word value ->
            Wasm.Call value

        AST.BuiltinPlus ->
            Wasm.Call addIntFn

        AST.BuiltinMinus ->
            Wasm.Call subIntFn

        AST.BuiltinEqual ->
            Wasm.Call eqIntFn
