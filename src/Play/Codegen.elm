module Play.Codegen exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.Data.Builtin as Builtin
import Play.Data.Type as Type exposing (Type)
import Play.TypeChecker as AST exposing (AST)
import Wasm


type alias TypeInformation =
    { id : Int
    , members : List ( String, Type )
    }



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
            { name = mulIntFn
            , exported = False
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


typeMeta : List AST.TypeDefinition -> Dict String TypeInformation
typeMeta types =
    types
        |> List.filterMap
            (\typeDef ->
                case typeDef of
                    AST.CustomTypeDef name members ->
                        Just
                            ( name
                            , { id = 0
                              , members = members
                              }
                            )

                    _ ->
                        Nothing
            )
        |> List.indexedMap
            (\idx ( name, def ) ->
                ( name
                , { def | id = idx }
                )
            )
        |> Dict.fromList


toWasmFuncDef : Dict String TypeInformation -> AST.WordDefinition -> Wasm.FunctionDef
toWasmFuncDef typeInfo def =
    let
        wasmImplementation =
            case def.implementation of
                AST.MultiImpl whens defaultImpl ->
                    [ multiFnToInstructions typeInfo def whens defaultImpl ]

                AST.SoloImpl impl ->
                    List.map (nodeToInstruction typeInfo) impl

        numberOfLocals =
            List.filterMap Wasm.maximumLocalIndex wasmImplementation
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
    in
    { name = def.name
    , exported = def.metadata.isEntryPoint
    , args = []
    , results = []
    , locals = List.repeat numberOfLocals Wasm.Int32
    , instructions = wasmImplementation
    }


multiFnToInstructions :
    Dict String TypeInformation
    -> AST.WordDefinition
    -> List ( Type, List AST.AstNode )
    -> List AST.AstNode
    -> Wasm.Instruction
multiFnToInstructions typeInfo def whens defaultImpl =
    let
        branches =
            List.foldl buildBranch (Wasm.Batch []) whens

        buildBranch ( type_, nodes ) ins =
            let
                typeId =
                    case type_ of
                        Type.Custom name ->
                            Dict.get name typeInfo
                                |> Maybe.map .id
                                |> Maybe.withDefault 0

                        _ ->
                            -- TODO: What if we get an Int here?
                            0
            in
            Wasm.Block
                [ ins
                , Wasm.Local_Get 0
                , Wasm.I32_Const typeId
                , Wasm.I32_NotEq
                , Wasm.BreakIf 0 -- Move to next branch unless theres a type match
                , Wasm.Batch (List.map (nodeToInstruction typeInfo) nodes)
                , Wasm.Return
                ]

        selfIndex =
            max 0 (List.length def.type_.input - 1)
    in
    Wasm.Batch
        [ Wasm.I32_Const selfIndex
        , Wasm.Call stackGetElementFn
        , Wasm.I32_Load
        , Wasm.Local_Set 0 -- store instance id in local
        , branches
        , Wasm.Batch (List.map (nodeToInstruction typeInfo) defaultImpl)
        ]


nodeToInstruction : Dict String TypeInformation -> AST.AstNode -> Wasm.Instruction
nodeToInstruction typeInfo node =
    case node of
        AST.IntLiteral value ->
            Wasm.Batch
                [ Wasm.I32_Const value
                , Wasm.Call stackPushFn
                ]

        AST.Word value _ ->
            Wasm.Call value

        AST.WordRef name ->
            Wasm.FunctionIndex name

        AST.ConstructType typeName ->
            case Dict.get typeName typeInfo of
                Just type_ ->
                    let
                        typeSize =
                            wasmPtrSize + (memberSize * wasmPtrSize)

                        memberSize =
                            List.length type_.members
                    in
                    Wasm.Batch
                        [ Wasm.I32_Const typeSize
                        , Wasm.Call allocFn
                        , Wasm.Local_Tee 0
                        , Wasm.I32_Const type_.id
                        , Wasm.I32_Store
                        , Wasm.I32_Const memberSize
                        , Wasm.Local_Set 1
                        , Wasm.Block
                            [ Wasm.Loop
                                [ Wasm.Local_Get 1
                                , Wasm.I32_EqZero
                                , Wasm.BreakIf 1
                                , Wasm.Local_Get 0
                                , Wasm.I32_Const wasmPtrSize
                                , Wasm.Local_Get 1
                                , Wasm.I32_Mul
                                , Wasm.I32_Add
                                , Wasm.Call stackPopFn
                                , Wasm.I32_Store
                                , Wasm.Local_Get 1
                                , Wasm.I32_Const 1
                                , Wasm.I32_Sub
                                , Wasm.Local_Set 1
                                , Wasm.Break 0
                                ]
                            ]
                        , Wasm.Local_Get 0
                        , Wasm.Call stackPushFn
                        ]

                Nothing ->
                    Debug.todo "This cannot happen."

        AST.SetMember typeName memberName memberType ->
            case Dict.get typeName typeInfo of
                Just type_ ->
                    let
                        typeSize =
                            wasmPtrSize + (memberSize * wasmPtrSize)

                        memberSize =
                            List.length type_.members
                    in
                    case getMemberType typeInfo typeName memberName of
                        Just memberIndex ->
                            Wasm.Batch
                                [ Wasm.Call swapFn -- Instance should now be at top of stack
                                , Wasm.Call stackPopFn
                                , Wasm.I32_Const typeSize
                                , Wasm.Call copyStructFn -- Return copy of instance
                                , Wasm.Local_Tee 0
                                , Wasm.I32_Const ((memberIndex + 1) * wasmPtrSize) -- Calculate member offset
                                , Wasm.I32_Add -- Calculate member address
                                , Wasm.Call stackPopFn -- Retrieve new value
                                , Wasm.I32_Store
                                , Wasm.Local_Get 0 -- Return instance
                                , Wasm.Call stackPushFn
                                ]

                        Nothing ->
                            Debug.todo "NOOOOO!"

                Nothing ->
                    Debug.todo "This cannot happen!"

        AST.GetMember typeName memberName memberType ->
            case getMemberType typeInfo typeName memberName of
                Just memberIndex ->
                    Wasm.Batch
                        [ Wasm.Call stackPopFn -- Get instance address
                        , Wasm.I32_Const ((memberIndex + 1) * wasmPtrSize) -- Calculate member offset
                        , Wasm.I32_Add -- Calculate member address
                        , Wasm.I32_Load -- Retrieve member
                        , Wasm.Call stackPushFn -- Push member onto stack
                        ]

                Nothing ->
                    Debug.todo "This cannot happen!"

        AST.Builtin builtin ->
            case builtin of
                Builtin.Plus ->
                    Wasm.Call addIntFn

                Builtin.Minus ->
                    Wasm.Call subIntFn

                Builtin.Multiply ->
                    Wasm.Call mulIntFn

                Builtin.Divide ->
                    Wasm.Call divIntFn

                Builtin.Equal ->
                    Wasm.Call eqIntFn

                Builtin.StackDuplicate ->
                    Wasm.Call dupFn

                Builtin.StackDrop ->
                    Wasm.Call dropFn

                Builtin.StackSwap ->
                    Wasm.Call swapFn

                Builtin.StackRightRotate ->
                    Wasm.Call rotFn

                Builtin.StackLeftRotate ->
                    Wasm.Call leftRotFn

                Builtin.Apply ->
                    Wasm.CallIndirect


getMemberType : Dict String TypeInformation -> String -> String -> Maybe Int
getMemberType typeInfoDict typeName memberName =
    Dict.get typeName typeInfoDict
        |> Maybe.map (List.indexedMap (\idx ( name, _ ) -> ( idx, name )) << .members)
        |> Maybe.andThen (List.find (\( _, name ) -> name == memberName))
        |> Maybe.map Tuple.first
