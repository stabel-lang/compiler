module Play.Codegen exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.Codegen.BaseModule as BaseModule
import Play.Data.Builtin as Builtin
import Play.Data.Type as Type exposing (Type)
import Play.TypeChecker as AST exposing (AST)
import Wasm


type alias TypeInformation =
    { id : Int
    , members : List ( String, Type )
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
        |> List.foldl Wasm.withFunction BaseModule.baseModule
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
    , isIndirectlyCalled = def.metadata.isQuoted
    , args = []
    , results = []
    , locals = List.repeat numberOfLocals Wasm.Int32
    , instructions = wasmImplementation
    }


multiFnToInstructions :
    Dict String TypeInformation
    -> AST.WordDefinition
    -> List ( AST.TypeMatch, List AST.AstNode )
    -> List AST.AstNode
    -> Wasm.Instruction
multiFnToInstructions typeInfo def whens defaultImpl =
    let
        branches =
            List.foldr buildBranch (Wasm.Batch []) whens

        buildBranch ( type_, nodes ) previousBranch =
            let
                testForInequality =
                    makeInequalityTest type_ 0

                makeInequalityTest t_ localIdx =
                    case t_ of
                        AST.TypeMatch (Type.Custom name) conditions ->
                            let
                                typeId =
                                    Dict.get name typeInfo
                                        |> Maybe.map .id
                                        |> Maybe.withDefault 0
                            in
                            Wasm.Batch
                                [ Wasm.Local_Get localIdx
                                , Wasm.I32_Load -- Load instance id
                                , Wasm.I32_Const typeId
                                , Wasm.I32_NotEq -- Types doesn't match?
                                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                                , conditions
                                    |> List.concatMap (conditionTest localIdx)
                                    |> Wasm.Batch
                                ]

                        _ ->
                            Debug.todo "Only supports custom types in when clauses"

                conditionTest localIdx ( fieldName, value ) =
                    case value of
                        AST.LiteralInt num ->
                            [ Wasm.Local_Get localIdx
                            , Wasm.Call BaseModule.stackPushFn
                            , Wasm.Call <| fieldName ++ ">"
                            , Wasm.Call BaseModule.stackPopFn
                            , Wasm.I32_Const num
                            , Wasm.I32_NotEq -- not same number?
                            , Wasm.BreakIf 0 -- move to next branch
                            ]

                        AST.LiteralType typ_ ->
                            case typ_ of
                                Type.Custom typeName ->
                                    let
                                        typeId =
                                            Dict.get typeName typeInfo
                                                |> Maybe.map .id
                                                |> Maybe.withDefault 0
                                    in
                                    [ Wasm.Local_Get localIdx
                                    , Wasm.Call BaseModule.stackPushFn
                                    , Wasm.Call <| fieldName ++ ">"
                                    , Wasm.Call BaseModule.stackPopFn
                                    , Wasm.I32_Load -- get type id
                                    , Wasm.I32_Const typeId
                                    , Wasm.I32_NotEq -- not same type?
                                    , Wasm.BreakIf 0 -- move to next branch
                                    ]

                                _ ->
                                    Debug.todo "oops"

                        AST.RecursiveMatch match ->
                            let
                                nextLocalIdx =
                                    localIdx + 1
                            in
                            [ Wasm.Local_Get localIdx
                            , Wasm.Call BaseModule.stackPushFn
                            , Wasm.Call <| fieldName ++ ">"
                            , Wasm.Call BaseModule.stackPopFn
                            , Wasm.Local_Set nextLocalIdx
                            , makeInequalityTest match nextLocalIdx
                            ]

                implementation =
                    nodes
                        |> List.map (nodeToInstruction typeInfo)
                        |> Wasm.Batch
            in
            Wasm.Block
                [ previousBranch
                , testForInequality
                , implementation
                , Wasm.Return
                ]

        selfIndex =
            max 0 (List.length def.type_.input - 1)
    in
    Wasm.Batch
        [ Wasm.I32_Const selfIndex
        , Wasm.Call BaseModule.stackGetElementFn
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
                , Wasm.Call BaseModule.stackPushFn
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
                            BaseModule.wasmPtrSize + (memberSize * BaseModule.wasmPtrSize)

                        memberSize =
                            List.length type_.members
                    in
                    Wasm.Batch
                        [ Wasm.I32_Const typeSize
                        , Wasm.Call BaseModule.allocFn
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
                                , Wasm.I32_Const BaseModule.wasmPtrSize
                                , Wasm.Local_Get 1
                                , Wasm.I32_Mul
                                , Wasm.I32_Add
                                , Wasm.Call BaseModule.stackPopFn
                                , Wasm.I32_Store
                                , Wasm.Local_Get 1
                                , Wasm.I32_Const 1
                                , Wasm.I32_Sub
                                , Wasm.Local_Set 1
                                , Wasm.Break 0
                                ]
                            ]
                        , Wasm.Local_Get 0
                        , Wasm.Call BaseModule.stackPushFn
                        ]

                Nothing ->
                    Debug.todo "This cannot happen."

        AST.SetMember typeName memberName memberType ->
            case Dict.get typeName typeInfo of
                Just type_ ->
                    let
                        typeSize =
                            BaseModule.wasmPtrSize + (memberSize * BaseModule.wasmPtrSize)

                        memberSize =
                            List.length type_.members
                    in
                    case getMemberType typeInfo typeName memberName of
                        Just memberIndex ->
                            Wasm.Batch
                                [ Wasm.Call BaseModule.swapFn -- Instance should now be at top of stack
                                , Wasm.Call BaseModule.stackPopFn
                                , Wasm.I32_Const typeSize
                                , Wasm.Call BaseModule.copyStructFn -- Return copy of instance
                                , Wasm.Local_Tee 0
                                , Wasm.I32_Const ((memberIndex + 1) * BaseModule.wasmPtrSize) -- Calculate member offset
                                , Wasm.I32_Add -- Calculate member address
                                , Wasm.Call BaseModule.stackPopFn -- Retrieve new value
                                , Wasm.I32_Store
                                , Wasm.Local_Get 0 -- Return instance
                                , Wasm.Call BaseModule.stackPushFn
                                ]

                        Nothing ->
                            Debug.todo "NOOOOO!"

                Nothing ->
                    Debug.todo "This cannot happen!"

        AST.GetMember typeName memberName memberType ->
            case getMemberType typeInfo typeName memberName of
                Just memberIndex ->
                    Wasm.Batch
                        [ Wasm.Call BaseModule.stackPopFn -- Get instance address
                        , Wasm.I32_Const ((memberIndex + 1) * BaseModule.wasmPtrSize) -- Calculate member offset
                        , Wasm.I32_Add -- Calculate member address
                        , Wasm.I32_Load -- Retrieve member
                        , Wasm.Call BaseModule.stackPushFn -- Push member onto stack
                        ]

                Nothing ->
                    Debug.todo "This cannot happen!"

        AST.Builtin builtin ->
            case builtin of
                Builtin.Plus ->
                    Wasm.Call BaseModule.addIntFn

                Builtin.Minus ->
                    Wasm.Call BaseModule.subIntFn

                Builtin.Multiply ->
                    Wasm.Call BaseModule.mulIntFn

                Builtin.Divide ->
                    Wasm.Call BaseModule.divIntFn

                Builtin.Equal ->
                    Wasm.Call BaseModule.eqIntFn

                Builtin.StackDuplicate ->
                    Wasm.Call BaseModule.dupFn

                Builtin.StackDrop ->
                    Wasm.Call BaseModule.dropFn

                Builtin.StackSwap ->
                    Wasm.Call BaseModule.swapFn

                Builtin.StackRightRotate ->
                    Wasm.Call BaseModule.rotFn

                Builtin.StackLeftRotate ->
                    Wasm.Call BaseModule.leftRotFn

                Builtin.Apply ->
                    Wasm.CallIndirect


getMemberType : Dict String TypeInformation -> String -> String -> Maybe Int
getMemberType typeInfoDict typeName memberName =
    Dict.get typeName typeInfoDict
        |> Maybe.map (List.indexedMap (\idx ( name, _ ) -> ( idx, name )) << .members)
        |> Maybe.andThen (List.find (\( _, name ) -> name == memberName))
        |> Maybe.map Tuple.first
