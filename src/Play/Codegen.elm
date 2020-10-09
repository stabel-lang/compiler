module Play.Codegen exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.Codegen.BaseModule as BaseModule
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.TypeChecker as AST exposing (AST)
import Wasm


type alias TypeInformation =
    { id : Int
    , members : List ( String, Type )
    }


type AstNode
    = IntLiteral Int
    | Word String WordType
    | WordRef String
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type
    | Builtin Builtin
    | PromoteInt Int



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
        |> List.map (toWasmFuncDef typeMetaDict ast)
        |> List.foldl Wasm.withFunction BaseModule.baseModule
        |> Ok


typeMeta : List AST.TypeDefinition -> Dict String TypeInformation
typeMeta types =
    types
        |> List.filterMap
            (\typeDef ->
                case typeDef of
                    AST.CustomTypeDef name _ _ members ->
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


toWasmFuncDef :
    Dict String TypeInformation
    -> AST
    -> AST.WordDefinition
    -> Wasm.FunctionDef
toWasmFuncDef typeInfo ast def =
    let
        wasmImplementation =
            case def.implementation of
                AST.SoloImpl impl ->
                    astNodesToInstructions typeInfo ast def impl

                AST.MultiImpl whens defaultImpl ->
                    [ multiFnToInstructions typeInfo ast def whens defaultImpl ]

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


astNodesToInstructions :
    Dict String TypeInformation
    -> AST
    -> AST.WordDefinition
    -> List AST.AstNode
    -> List Wasm.Instruction
astNodesToInstructions typeInfo ast def astNodes =
    astNodes
        |> List.foldl (astNodeToCodegenNode ast) ( def.type_.input, [] )
        |> Tuple.second
        |> List.reverse
        |> List.map (nodeToInstruction typeInfo)


astNodeToCodegenNode :
    AST
    -> AST.AstNode
    -> ( List Type, List AstNode )
    -> ( List Type, List AstNode )
astNodeToCodegenNode ast node ( stack, result ) =
    let
        newNode =
            case node of
                AST.IntLiteral _ val ->
                    IntLiteral val

                AST.Word _ name type_ ->
                    Word name type_

                AST.WordRef _ name ->
                    WordRef name

                AST.ConstructType typeName ->
                    ConstructType typeName

                AST.SetMember typeName memberName type_ ->
                    SetMember typeName memberName type_

                AST.GetMember typeName memberName type_ ->
                    GetMember typeName memberName type_

                AST.Builtin _ builtin ->
                    Builtin builtin

        nodeType =
            case node of
                AST.IntLiteral _ _ ->
                    { input = []
                    , output = [ Type.Int ]
                    }

                AST.Word _ name type_ ->
                    type_

                AST.WordRef _ name ->
                    case Dict.get name ast.words of
                        Just def ->
                            { input = []
                            , output = [ Type.Quotation def.type_ ]
                            }

                        Nothing ->
                            Debug.todo "help"

                AST.ConstructType typeName ->
                    case Dict.get typeName ast.types of
                        Just (AST.CustomTypeDef _ _ gens members) ->
                            { input = List.map Tuple.second members
                            , output = [ typeFromTypeDef typeName gens ]
                            }

                        _ ->
                            Debug.todo "help"

                AST.SetMember typeName _ memberType ->
                    case Dict.get typeName ast.types of
                        Just (AST.CustomTypeDef _ _ gens _) ->
                            let
                                type_ =
                                    typeFromTypeDef typeName gens
                            in
                            { input = [ type_, memberType ]
                            , output = [ type_ ]
                            }

                        _ ->
                            Debug.todo "help"

                AST.GetMember typeName _ memberType ->
                    case Dict.get typeName ast.types of
                        Just (AST.CustomTypeDef _ _ gens _) ->
                            let
                                type_ =
                                    typeFromTypeDef typeName gens
                            in
                            { input = [ type_ ]
                            , output = [ memberType ]
                            }

                        _ ->
                            Debug.todo "help"

                AST.Builtin _ builtin ->
                    Builtin.wordType builtin

        typeFromTypeDef typeName gens =
            if List.isEmpty gens then
                Type.Custom typeName

            else
                Type.CustomGeneric typeName (List.map Type.Generic gens)

        intsToPromote =
            List.map2 Tuple.pair (List.reverse stack) (List.reverse nodeType.input)
                |> List.indexedMap (\i ( l, r ) -> ( i, l, r ))
                |> List.filterMap maybePromoteInt
                |> maybeCons maybePromoteLeadingInt

        maybePromoteInt ( idx, leftType, rightType ) =
            case ( leftType, rightType ) of
                ( Type.Int, Type.Union _ ) ->
                    Just (PromoteInt idx)

                _ ->
                    Nothing

        maybePromoteLeadingInt =
            case ( List.head <| List.reverse stack, isMultiWord newNode, List.head <| List.reverse nodeType.input ) of
                ( Just Type.Int, True, Just (Type.Union _) ) ->
                    -- Already handled by maybePromoteInt
                    Nothing

                ( Just Type.Int, True, Just _ ) ->
                    Just (PromoteInt 0)

                _ ->
                    Nothing

        isMultiWord possibleMultiWordNode =
            case possibleMultiWordNode of
                Word name _ ->
                    case Dict.get name ast.words of
                        Just def ->
                            case def.implementation of
                                AST.SoloImpl _ ->
                                    False

                                AST.MultiImpl _ _ ->
                                    True

                        Nothing ->
                            False

                _ ->
                    False

        maybeCons maybeBox list =
            case maybeBox of
                Just value ->
                    value :: list

                Nothing ->
                    list

        newStack =
            List.reverse stack
                |> List.drop (List.length nodeType.input)
                |> (\s -> List.reverse nodeType.output ++ s)
                |> List.reverse
    in
    ( newStack
    , newNode :: (intsToPromote ++ result)
    )


multiFnToInstructions :
    Dict String TypeInformation
    -> AST
    -> AST.WordDefinition
    -> List ( AST.TypeMatch, List AST.AstNode )
    -> List AST.AstNode
    -> Wasm.Instruction
multiFnToInstructions typeInfo ast def whens defaultImpl =
    let
        branches =
            List.foldl buildBranch (Wasm.Batch []) whens

        buildBranch ( type_, nodes ) previousBranch =
            let
                testForInequality =
                    makeInequalityTest type_ 0

                makeInequalityTest t_ localIdx =
                    case t_ of
                        AST.TypeMatch _ Type.Int conditions ->
                            Wasm.Batch
                                [ Wasm.Local_Get localIdx
                                , Wasm.I32_Load -- Load instance id
                                , Wasm.I32_Const BaseModule.intBoxId
                                , Wasm.I32_NotEq -- Types doesn't match?
                                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                                , conditions
                                    |> List.concatMap (matchingIntTest localIdx)
                                    |> Wasm.Batch
                                , Wasm.I32_Const selfIndex
                                , Wasm.Call BaseModule.demoteIntFn
                                ]

                        AST.TypeMatch _ (Type.Custom name) conditions ->
                            whenSetup localIdx name conditions

                        AST.TypeMatch _ (Type.CustomGeneric name _) conditions ->
                            whenSetup localIdx name conditions

                        _ ->
                            Debug.todo "Only supports custom types in when clauses"

                whenSetup localIdx typeName conditions =
                    let
                        typeId =
                            Dict.get typeName typeInfo
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

                matchingIntTest localIdx ( _, astValue ) =
                    let
                        value =
                            case astValue of
                                AST.LiteralInt num ->
                                    num

                                _ ->
                                    0
                    in
                    [ Wasm.Local_Get localIdx
                    , Wasm.I32_Const BaseModule.wasmPtrSize
                    , Wasm.I32_Add
                    , Wasm.I32_Load -- int value
                    , Wasm.I32_Const value
                    , Wasm.I32_NotEq -- not same number?
                    , Wasm.BreakIf 0 -- move to next branch
                    ]

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
                        |> astNodesToInstructions typeInfo ast def
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
        , Wasm.Batch (astNodesToInstructions typeInfo ast def defaultImpl)
        ]


nodeToInstruction : Dict String TypeInformation -> AstNode -> Wasm.Instruction
nodeToInstruction typeInfo node =
    case node of
        IntLiteral value ->
            Wasm.Batch
                [ Wasm.I32_Const value
                , Wasm.Call BaseModule.stackPushFn
                ]

        Word value _ ->
            Wasm.Call value

        WordRef name ->
            Wasm.FunctionIndex name

        ConstructType typeName ->
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

        SetMember typeName memberName memberType ->
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

        GetMember typeName memberName memberType ->
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

        Builtin builtin ->
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

        PromoteInt stackPos ->
            Wasm.Batch
                [ Wasm.I32_Const stackPos
                , Wasm.Call BaseModule.promoteIntFn
                ]


getMemberType : Dict String TypeInformation -> String -> String -> Maybe Int
getMemberType typeInfoDict typeName memberName =
    Dict.get typeName typeInfoDict
        |> Maybe.map (List.indexedMap (\idx ( name, _ ) -> ( idx, name )) << .members)
        |> Maybe.andThen (List.find (\( _, name ) -> name == memberName))
        |> Maybe.map Tuple.first
