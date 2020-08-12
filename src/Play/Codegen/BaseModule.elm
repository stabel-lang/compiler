module Play.Codegen.BaseModule exposing (..)

import Wasm



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


intBoxId : Int
intBoxId =
    -1



-- Bultin function names


allocFn : String
allocFn =
    "__alloc"


copyStructFn : String
copyStructFn =
    "__copy_str"


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


mulIntFn : String
mulIntFn =
    "__mul_i32"


divIntFn : String
divIntFn =
    "__div_i32"


eqIntFn : String
eqIntFn =
    "__eq_i32"


dupFn : String
dupFn =
    "__duplicate"


dropFn : String
dropFn =
    "__drop"


swapFn : String
swapFn =
    "__swap"


rotFn : String
rotFn =
    "__rotate"


leftRotFn : String
leftRotFn =
    "__left_rotate"


stackGetElementFn : String
stackGetElementFn =
    "__stack_get"


stackReplaceElementFn : String
stackReplaceElementFn =
    "__stack_replace"


promoteIntFn : String
promoteIntFn =
    "__promote_int"


demoteIntFn : String
demoteIntFn =
    "__demote_int"



-- Base module


baseModule : Wasm.Module
baseModule =
    Wasm.initModule
        |> Wasm.withImport "host" "memory" (Wasm.Memory 1 Nothing)
        |> Wasm.withStartFunction
            { name = "__initialize"
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = allocFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = copyStructFn
            , exported = False
            , isIndirectlyCalled = False
            , args = [ Wasm.Int32, Wasm.Int32 ]
            , results = [ Wasm.Int32 ]
            , locals = [ Wasm.Int32, Wasm.Int32 ]
            , instructions =
                [ Wasm.Local_Get 1 -- Size in bytes
                , Wasm.Call allocFn
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
        |> Wasm.withFunction
            { name = stackPushFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = stackPopFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = dupFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , results = []
            , locals = [ Wasm.Int32 ]
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Local_Tee 0
                , Wasm.Local_Get 0
                , Wasm.Call stackPushFn
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = dropFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , results = []
            , locals = []
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Drop
                ]
            }
        |> Wasm.withFunction
            { name = swapFn
            , exported = False
            , isIndirectlyCalled = False
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
            { name = rotFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , results = []
            , locals = [ Wasm.Int32, Wasm.Int32, Wasm.Int32 ]
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Local_Set 0 -- c
                , Wasm.Call stackPopFn
                , Wasm.Local_Set 1 -- b
                , Wasm.Call stackPopFn
                , Wasm.Local_Set 2 -- a
                , Wasm.Local_Get 0
                , Wasm.Call stackPushFn
                , Wasm.Local_Get 2
                , Wasm.Call stackPushFn
                , Wasm.Local_Get 1
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = leftRotFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , results = []
            , locals = [ Wasm.Int32, Wasm.Int32, Wasm.Int32 ]
            , instructions =
                [ Wasm.Call stackPopFn
                , Wasm.Local_Set 0 -- c
                , Wasm.Call stackPopFn
                , Wasm.Local_Set 1 -- b
                , Wasm.Call stackPopFn
                , Wasm.Local_Set 2 -- a
                , Wasm.Local_Get 1
                , Wasm.Call stackPushFn
                , Wasm.Local_Get 0
                , Wasm.Call stackPushFn
                , Wasm.Local_Get 2
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = addIntFn
            , exported = False
            , isIndirectlyCalled = False
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
            , isIndirectlyCalled = False
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
            { name = mulIntFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , locals = []
            , results = []
            , instructions =
                [ Wasm.Call swapFn
                , Wasm.Call stackPopFn
                , Wasm.Call stackPopFn
                , Wasm.I32_Mul
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = divIntFn
            , exported = False
            , isIndirectlyCalled = False
            , args = []
            , locals = []
            , results = []
            , instructions =
                [ Wasm.Call swapFn
                , Wasm.Call stackPopFn
                , Wasm.Call stackPopFn
                , Wasm.I32_Div
                , Wasm.Call stackPushFn
                ]
            }
        |> Wasm.withFunction
            { name = eqIntFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = stackGetElementFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = stackReplaceElementFn
            , exported = False
            , isIndirectlyCalled = False
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
        |> Wasm.withFunction
            { name = promoteIntFn
            , exported = False
            , isIndirectlyCalled = False
            , args = [ Wasm.Int32 ]
            , results = []
            , locals = [ Wasm.Int32 ]
            , instructions =
                let
                    typeSize =
                        -- type descriptor and value
                        wasmPtrSize * 2
                in
                [ Wasm.I32_Const typeSize
                , Wasm.Call allocFn
                , Wasm.Local_Tee 1
                , Wasm.I32_Const intBoxId
                , Wasm.I32_Store
                , Wasm.Local_Get 1
                , Wasm.I32_Const wasmPtrSize
                , Wasm.I32_Add
                , Wasm.Local_Get 0
                , Wasm.Call stackGetElementFn
                , Wasm.I32_Store
                , Wasm.Local_Get 0
                , Wasm.Local_Get 1
                , Wasm.Call stackReplaceElementFn
                ]
            }
        |> Wasm.withFunction
            { name = demoteIntFn
            , exported = False
            , isIndirectlyCalled = False
            , args = [ Wasm.Int32 ]
            , results = []
            , locals = []
            , instructions =
                [ Wasm.Local_Get 0
                , Wasm.Local_Get 0
                , Wasm.Call stackGetElementFn
                , Wasm.I32_Const wasmPtrSize
                , Wasm.I32_Add
                , Wasm.I32_Load
                , Wasm.Call stackReplaceElementFn
                ]
            }
