module Stabel.Codegen.BaseModule exposing (..)

import Stabel.Wasm as Wasm



-- Constants


wasmPtrSize : Int
wasmPtrSize =
    4


stackCapacityOffset : Int
stackCapacityOffset =
    0


stackPositionOffset : Int
stackPositionOffset =
    wasmPtrSize


defaultStackSize : Int
defaultStackSize =
    1024


initialHeapPositionOffset : Int
initialHeapPositionOffset =
    stackPositionOffset + wasmPtrSize


firstAvailableFunctionId : Int
firstAvailableFunctionId =
    20



-- Bultin function names


allocFn : String
allocFn =
    "__alloc"


callAllocFn : Wasm.Instruction
callAllocFn =
    Wasm.Call 1 allocFn


copyStructFn : String
copyStructFn =
    "__copy_struct"


callCopyStructFn : Wasm.Instruction
callCopyStructFn =
    Wasm.Call 2 copyStructFn


stackPushFn : String
stackPushFn =
    "__stack_push"


callStackPushFn : Wasm.Instruction
callStackPushFn =
    Wasm.Call 3 stackPushFn


stackPopFn : String
stackPopFn =
    "__stack_pop"


callStackPopFn : Wasm.Instruction
callStackPopFn =
    Wasm.Call 4 stackPopFn


dupFn : String
dupFn =
    "__duplicate"


callDupFn : Wasm.Instruction
callDupFn =
    Wasm.Call 5 dupFn


dropFn : String
dropFn =
    "__drop"


callDropFn : Wasm.Instruction
callDropFn =
    Wasm.Call 6 dropFn


swapFn : String
swapFn =
    "__swap"


callSwapFn : Wasm.Instruction
callSwapFn =
    Wasm.Call 7 swapFn


rotFn : String
rotFn =
    "__rotate"


callRotFn : Wasm.Instruction
callRotFn =
    Wasm.Call 8 rotFn


leftRotFn : String
leftRotFn =
    "__left_rotate"


callLeftRotFn : Wasm.Instruction
callLeftRotFn =
    Wasm.Call 9 leftRotFn


addIntFn : String
addIntFn =
    "__add_i32"


callAddIntFn : Wasm.Instruction
callAddIntFn =
    Wasm.Call 10 addIntFn


subIntFn : String
subIntFn =
    "__sub_i32"


callSubIntFn : Wasm.Instruction
callSubIntFn =
    Wasm.Call 11 subIntFn


mulIntFn : String
mulIntFn =
    "__mul_i32"


callMulIntFn : Wasm.Instruction
callMulIntFn =
    Wasm.Call 12 mulIntFn


divIntFn : String
divIntFn =
    "__div_i32"


callDivIntFn : Wasm.Instruction
callDivIntFn =
    Wasm.Call 13 divIntFn


eqIntFn : String
eqIntFn =
    "__eq_i32"


callEqIntFn : Wasm.Instruction
callEqIntFn =
    Wasm.Call 14 eqIntFn


stackGetElementFn : String
stackGetElementFn =
    "__stack_get"


callStackGetElementFn : Wasm.Instruction
callStackGetElementFn =
    Wasm.Call 15 stackGetElementFn


stackReplaceElementFn : String
stackReplaceElementFn =
    "__stack_replace"


callStackReplaceElementFn : Wasm.Instruction
callStackReplaceElementFn =
    Wasm.Call 16 stackReplaceElementFn


boxFn : String
boxFn =
    "__box"


callBoxFn : Wasm.Instruction
callBoxFn =
    Wasm.Call 17 boxFn


unboxFn : String
unboxFn =
    "__unbox"


callUnboxFn : Wasm.Instruction
callUnboxFn =
    Wasm.Call 18 unboxFn


execInlineFn : String
execInlineFn =
    "__exec_inline"


callExecInlineFn : Wasm.Instruction
callExecInlineFn =
    Wasm.Call 19 execInlineFn



-- Base module


baseModule : Wasm.Module
baseModule =
    let
        withoutFunctions =
            Wasm.initModule
                |> Wasm.withImport "host" "memory" (Wasm.Memory 1 Nothing)
                |> Wasm.withStartFunction 0
    in
    List.foldl Wasm.withFunction withoutFunctions baseFunctions


baseFunctions : List Wasm.FunctionDefinition
baseFunctions =
    [ { id = 0
      , name = "__initialize"
      , args = []
      , results = []
      , locals = []
      , instructions =
            [ Wasm.I32_Const stackCapacityOffset
            , Wasm.I32_Const defaultStackSize
            , Wasm.I32_Store
            , Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Const (wasmPtrSize * 3)
            , Wasm.I32_Store
            , Wasm.I32_Const initialHeapPositionOffset
            , Wasm.I32_Const (defaultStackSize + wasmPtrSize)
            , Wasm.I32_Store
            ]
      }
    , { id = 1
      , name = allocFn
      , args = [ Wasm.Int32 ]
      , results = [ Wasm.Int32 ]
      , locals = [ Wasm.Int32 ]
      , instructions =
            [ Wasm.I32_Const initialHeapPositionOffset
            , Wasm.I32_Const initialHeapPositionOffset
            , Wasm.I32_Load
            , Wasm.Local_Tee 1
            , Wasm.Local_Get 0
            , Wasm.I32_Add
            , Wasm.I32_Store
            , Wasm.Local_Get 1
            ]
      }
    , { id = 2
      , name = copyStructFn
      , args = [ Wasm.Int32, Wasm.Int32 ]
      , results = [ Wasm.Int32 ]
      , locals = [ Wasm.Int32, Wasm.Int32 ]
      , instructions =
            [ Wasm.Local_Get 1 -- Size in bytes
            , callAllocFn
            , Wasm.Local_Set 2 -- Save output instance
            , Wasm.Block
                [ Wasm.Loop
                    [ Wasm.Local_Get 1
                    , Wasm.I32_EqZero
                    , Wasm.BreakIf 1 -- break out of loop
                    , Wasm.Local_Get 1
                    , Wasm.I32_Const wasmPtrSize
                    , Wasm.I32_Sub
                    , Wasm.Local_Set 1 -- Decreased pointer size
                    , Wasm.Local_Get 0 -- Source struct
                    , Wasm.Local_Get 1
                    , Wasm.I32_Add
                    , Wasm.I32_Load -- Get a byte from source struct
                    , Wasm.Local_Set 3 -- Save byte to copy
                    , Wasm.Local_Get 2 -- Dest struct
                    , Wasm.Local_Get 1
                    , Wasm.I32_Add
                    , Wasm.Local_Get 3
                    , Wasm.I32_Store -- Copy byte from source to dest struct
                    , Wasm.Break 0 -- loop
                    ]
                ]
            , Wasm.Local_Get 2
            ]
      }
    , { id = 3
      , name = stackPushFn
      , args = [ Wasm.Int32 ]
      , results = []
      , locals = [ Wasm.Int32 ]
      , instructions =
            [ Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Load -- Get current stack position
            , Wasm.Local_Tee 1
            , Wasm.Local_Get 0
            , Wasm.I32_Store -- Store input value in stack
            , Wasm.I32_Const stackPositionOffset
            , Wasm.Local_Get 1
            , Wasm.I32_Const wasmPtrSize
            , Wasm.I32_Add -- Bump stack size
            , Wasm.I32_Store -- Save new stack position
            ]
      }
    , { id = 4
      , name = stackPopFn
      , args = []
      , results = [ Wasm.Int32 ]
      , locals = [ Wasm.Int32 ]
      , instructions =
            [ Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Load
            , Wasm.I32_Const wasmPtrSize
            , Wasm.I32_Sub
            , Wasm.Local_Tee 0 -- Save new stack position in local register
            , Wasm.I32_Store -- save new stack position in global variable
            , Wasm.Local_Get 0
            , Wasm.I32_Load -- Load element at top of the stack
            ]
      }
    , { id = 5
      , name = dupFn
      , args = []
      , results = []
      , locals = [ Wasm.Int32 ]
      , instructions =
            [ callStackPopFn
            , Wasm.Local_Tee 0
            , Wasm.Local_Get 0
            , callStackPushFn
            , callStackPushFn
            ]
      }
    , { id = 6
      , name = dropFn
      , args = []
      , results = []
      , locals = []
      , instructions =
            [ callStackPopFn
            , Wasm.Drop
            ]
      }
    , { id = 7
      , name = swapFn
      , args = []
      , results = []
      , locals = [ Wasm.Int32 ]
      , instructions =
            [ callStackPopFn
            , Wasm.Local_Set 0
            , callStackPopFn
            , Wasm.Local_Get 0
            , callStackPushFn
            , callStackPushFn
            ]
      }
    , { id = 8
      , name = rotFn
      , args = []
      , results = []
      , locals = [ Wasm.Int32, Wasm.Int32, Wasm.Int32 ]
      , instructions =
            [ callStackPopFn
            , Wasm.Local_Set 0 -- c
            , callStackPopFn
            , Wasm.Local_Set 1 -- b
            , callStackPopFn
            , Wasm.Local_Set 2 -- a
            , Wasm.Local_Get 0
            , callStackPushFn
            , Wasm.Local_Get 2
            , callStackPushFn
            , Wasm.Local_Get 1
            , callStackPushFn
            ]
      }
    , { id = 9
      , name = leftRotFn
      , args = []
      , results = []
      , locals = [ Wasm.Int32, Wasm.Int32, Wasm.Int32 ]
      , instructions =
            [ callStackPopFn
            , Wasm.Local_Set 0 -- c
            , callStackPopFn
            , Wasm.Local_Set 1 -- b
            , callStackPopFn
            , Wasm.Local_Set 2 -- a
            , Wasm.Local_Get 1
            , callStackPushFn
            , Wasm.Local_Get 0
            , callStackPushFn
            , Wasm.Local_Get 2
            , callStackPushFn
            ]
      }
    , { id = 10
      , name = addIntFn
      , args = []
      , results = []
      , locals = []
      , instructions =
            [ callSwapFn
            , callStackPopFn
            , callStackPopFn
            , Wasm.I32_Add
            , callStackPushFn
            ]
      }
    , { id = 11
      , name = subIntFn
      , args = []
      , locals = []
      , results = []
      , instructions =
            [ callSwapFn
            , callStackPopFn
            , callStackPopFn
            , Wasm.I32_Sub
            , callStackPushFn
            ]
      }
    , { id = 12
      , name = mulIntFn
      , args = []
      , locals = []
      , results = []
      , instructions =
            [ callSwapFn
            , callStackPopFn
            , callStackPopFn
            , Wasm.I32_Mul
            , callStackPushFn
            ]
      }
    , { id = 13
      , name = divIntFn
      , args = []
      , locals = []
      , results = []
      , instructions =
            [ callSwapFn
            , callStackPopFn
            , callStackPopFn
            , Wasm.I32_Div
            , callStackPushFn
            ]
      }
    , { id = 14
      , name = eqIntFn
      , args = []
      , locals = []
      , results = []
      , instructions =
            [ callStackPopFn
            , callStackPopFn
            , Wasm.I32_Eq
            , callStackPushFn
            ]
      }
    , { id = 15
      , name = stackGetElementFn
      , args = [ Wasm.Int32 ]
      , results = [ Wasm.Int32 ]
      , locals = []
      , instructions =
            [ Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Load
            , Wasm.I32_Const wasmPtrSize
            , Wasm.Local_Get 0 -- read offset
            , Wasm.I32_Const 1
            , Wasm.I32_Add -- add one to offset
            , Wasm.I32_Mul -- offset * ptrSize
            , Wasm.I32_Sub -- stackPosition - ptrOffset
            , Wasm.I32_Load
            ]
      }
    , { id = 16
      , name = stackReplaceElementFn
      , args = [ Wasm.Int32, Wasm.Int32 ]
      , results = []
      , locals = []
      , instructions =
            [ Wasm.I32_Const stackPositionOffset
            , Wasm.I32_Load
            , Wasm.I32_Const wasmPtrSize
            , Wasm.Local_Get 0 -- read offset
            , Wasm.I32_Const 1
            , Wasm.I32_Add -- add one to offset
            , Wasm.I32_Mul -- offset * ptrSize
            , Wasm.I32_Sub -- stackPosition - ptrOffset
            , Wasm.Local_Get 1
            , Wasm.I32_Store
            ]
      }
    , { id = 17
      , name = boxFn
      , args = [ Wasm.Int32, Wasm.Int32 ]
      , results = []
      , locals = [ Wasm.Int32 ]
      , instructions =
            let
                typeSize =
                    -- type descriptor and value
                    wasmPtrSize * 2
            in
            [ Wasm.I32_Const typeSize
            , Wasm.Call 1 allocFn
            , Wasm.Local_Tee 2
            , Wasm.Local_Get 1
            , Wasm.I32_Store
            , Wasm.Local_Get 2
            , Wasm.I32_Const wasmPtrSize
            , Wasm.I32_Add
            , Wasm.Local_Get 0
            , callStackGetElementFn
            , Wasm.I32_Store
            , Wasm.Local_Get 0
            , Wasm.Local_Get 2
            , callStackReplaceElementFn
            ]
      }
    , { id = 18
      , name = unboxFn
      , args = [ Wasm.Int32 ]
      , results = []
      , locals = []
      , instructions =
            [ Wasm.Local_Get 0
            , Wasm.Local_Get 0
            , callStackGetElementFn
            , Wasm.I32_Const wasmPtrSize
            , Wasm.I32_Add
            , Wasm.I32_Load
            , callStackReplaceElementFn
            ]
      }
    , { id = 19
      , name = execInlineFn
      , args = []
      , results = []
      , locals = []
      , instructions =
            [ callStackPopFn
            , Wasm.CallIndirect
            ]
      }
    ]
