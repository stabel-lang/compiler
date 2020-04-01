module Play.Codegen exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.TypeChecker as AST exposing (AST)
import Wasm



-- Constants


wasmPtrSize : Int
wasmPtrSize =
    4



-- Bultin function names


stackPushFn : String
stackPushFn =
    "__stack_push"


stackPopFn : String
stackPopFn =
    "__stack_pop"


addIntFn : String
addIntFn =
    "__add_i32"


subIntFn : String
subIntFn =
    "__sub_i32"


eqIntFn : String
eqIntFn =
    "__eq_i32"


swapFn : String
swapFn =
    "__swap"



-- Base module


baseModule : Wasm.Module
baseModule =
    Wasm.initModule
        |> Wasm.withImport "host" "memory" (Wasm.Memory 1 Nothing)
        |> Wasm.withStartFunction
            { name = "__initialize"
            , exported = False
            , args = []
            , results = []
            , locals = []
            , instructions =
                [ Wasm.I32_Const 0
                , Wasm.I32_Const 0
                , Wasm.I32_Store
                ]
            }
        |> Wasm.withFunction
            { name = stackPushFn
            , exported = False
            , args = [ Wasm.Int32 ]
            , results = []
            , locals = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                , Wasm.I32_Load -- Get current stack position
                , Wasm.I32_Const wasmPtrSize
                , Wasm.I32_Add -- Bump stack size
                , Wasm.Local_Set 1 -- Store new stack size
                , Wasm.I32_Const 0
                , Wasm.Local_Get 1
                , Wasm.I32_Store -- Store new stack size
                , Wasm.Local_Get 1
                , Wasm.Local_Get 0
                , Wasm.I32_Store -- Store input value in new stack position
                ]
            }
        |> Wasm.withFunction
            { name = stackPopFn
            , exported = False
            , args = []
            , results = [ Wasm.Int32 ]
            , locals = [ Wasm.Int32, Wasm.Int32 ]
            , instructions =
                [ Wasm.I32_Const 0
                , Wasm.I32_Load -- Get current stack position
                , Wasm.Local_Tee 0
                , Wasm.I32_Load
                , Wasm.Local_Set 1 -- Store item at top of stack in local 1
                , Wasm.Local_Get 0 -- Get stack position again
                , Wasm.I32_Const wasmPtrSize
                , Wasm.I32_Sub
                , Wasm.Local_Set 0 -- Store decreased stack position
                , Wasm.I32_Const 0
                , Wasm.Local_Get 0
                , Wasm.I32_Store
                , Wasm.Local_Get 1
                ]
            }
        |> Wasm.withFunction
            { name = swapFn
            , exported = False
            , args = []
            , results = []
            , locals = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Local_Set 0
                , Wasm.Call stackPopFn
                , Wasm.Local_Get 0
                , Wasm.Call stackPushFn
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = addIntFn
            , exported = False
            , args = []
            , results = []
            , locals = []
            , instructions =
                [ Wasm.Call swapFn
                , Wasm.Call stackPopFn
                , Wasm.Call stackPopFn
                , Wasm.I32_Add
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = subIntFn
            , exported = False
            , args = []
            , locals = []
            , results = []
            , instructions =
                [ Wasm.Call swapFn
                , Wasm.Call stackPopFn
                , Wasm.Call stackPopFn
                , Wasm.I32_Sub
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = eqIntFn
            , exported = False
            , args = []
            , locals = []
            , results = []
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Call stackPopFn
                , Wasm.I32_Eq
                , Wasm.Call stackPushFn
                ]
            }



-- Codegen


codegen : AST -> Result () Wasm.Module
codegen ast =
    let
        typeMetaDict =
            ast.types
                |> Dict.values
                |> typeMeta
    in
    ast.words
        |> Dict.values
        |> List.map (toWasmFuncDef typeMetaDict)
        |> List.foldl Wasm.withFunction baseModule
        |> Ok


typeMeta : List AST.TypeDefinition -> Dict String Int
typeMeta types =
    types
        |> List.indexedMap (\idx typeDef -> ( typeDef.name, idx ))
        |> Dict.fromList


toWasmFuncDef : Dict String Int -> AST.WordDefinition -> Wasm.FunctionDef
toWasmFuncDef typeInfo def =
    let
        wasmImplementation =
            List.map (nodeToInstruction typeInfo) def.implementation
    in
    { name = def.name
    , exported = def.metadata.isEntryPoint
    , args = []
    , results = []
    , locals = []
    , instructions = wasmImplementation
    }


nodeToInstruction : Dict String Int -> AST.AstNode -> Wasm.Instruction
nodeToInstruction typeInfo node =
    case node of
        AST.IntLiteral value ->
            Wasm.Batch
                [ Wasm.I32_Const value
                , Wasm.Call stackPushFn
                ]

        AST.Word value _ ->
            Wasm.Call value

        AST.ConstructType typeName ->
            case Dict.get typeName typeInfo of
                Just typeId ->
                    Wasm.Batch
                        [ Wasm.I32_Const typeId
                        , Wasm.Call stackPushFn
                        ]

                Nothing ->
                    Debug.todo "This cannot happen."

        AST.BuiltinPlus ->
            Wasm.Call addIntFn

        AST.BuiltinMinus ->
            Wasm.Call subIntFn

        AST.BuiltinEqual ->
            Wasm.Call eqIntFn
