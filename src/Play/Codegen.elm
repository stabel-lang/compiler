module Play.Codegen exposing (..)

import List.Extra as List
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
            , exported = False
            , args = []
            , results = []
            , instructions =
                [ Wasm.NoOp ]
            }
        |> Wasm.withFunction
            { name = stackPushFn
            , exported = False
            , args = [ Wasm.Int32 ]
            , results = []
            , instructions =
                [ Wasm.NoOp
                ]
            }
        |> Wasm.withFunction
            { name = stackPopFn
            , exported = False
            , args = []
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = pushIntfn
            , exported = False
            , args = [ Wasm.Int32, Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = addIntFn
            , exported = False
            , args = [ Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = subIntFn
            , exported = False
            , args = [ Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }
        |> Wasm.withFunction
            { name = eqIntFn
            , exported = False
            , args = [ Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                ]
            }


codegen : List AST.Definition -> Result () Wasm.Module
codegen ast =
    ast
        |> List.map toWasmFuncDef
        |> List.foldl Wasm.withFunction baseModule
        |> Ok


toWasmFuncDef : AST.Definition -> Wasm.FunctionDef
toWasmFuncDef def =
    let
        isEntryPoint =
            case List.find (\( mKey, _ ) -> mKey == "entry") def.metadata of
                Just _ ->
                    True

                Nothing ->
                    False

        wasmImplementation =
            List.map nodeToInstruction def.implementation
    in
    { name = def.name
    , exported = isEntryPoint
    , args = [ Wasm.Int32 ]
    , results = [ Wasm.Int32 ]
    , instructions = wasmImplementation
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
