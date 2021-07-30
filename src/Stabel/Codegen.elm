module Stabel.Codegen exposing (run)

import Dict exposing (Dict)
import List.Extra as List
import Set exposing (Set)
import Stabel.Codegen.BaseModule as BaseModule
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.Type as Type exposing (FunctionType, Type)
import Stabel.Qualifier exposing (TypeDefinitionMembers(..))
import Stabel.TypeChecker as AST exposing (AST)
import Wasm


type alias TypeInformation =
    { id : Int
    , members : List ( String, Type )
    }


type AstNode
    = IntLiteral Int
    | Word AST.FunctionDefinition FunctionType
    | WordRef AST.FunctionDefinition
    | ConstructType AST.TypeDefinition
    | SetMember AST.TypeDefinition String Int Type
    | GetMember AST.TypeDefinition String Int Type
    | Builtin Builtin
    | Box Int Int -- stackIdx typeId



-- Codegen


run : Set String -> AST -> Wasm.Module
run exportedFunctions ast =
    let
        typeMetaDict =
            ast.types
                |> Dict.values
                |> typeMeta
    in
    ast.functions
        |> Dict.values
        |> List.map (toWasmFuncDef typeMetaDict ast exportedFunctions)
        |> List.foldl Wasm.withFunction BaseModule.baseModule


typeMeta : List AST.TypeDefinition -> Dict String TypeInformation
typeMeta types =
    types
        |> List.filterMap
            (\typeDef ->
                case typeDef.members of
                    StructMembers members ->
                        Just
                            ( typeDef.name
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
    -> Set String
    -> AST.FunctionDefinition
    -> Wasm.FunctionDef
toWasmFuncDef typeInfo ast exportedFunctions def =
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
    , exported = Set.member def.name exportedFunctions
    , isIndirectlyCalled = def.isInline
    , args = []
    , results = []
    , locals = List.repeat numberOfLocals Wasm.Int32
    , instructions = wasmImplementation
    }


astNodesToInstructions :
    Dict String TypeInformation
    -> AST
    -> AST.FunctionDefinition
    -> List AST.AstNode
    -> List Wasm.Instruction
astNodesToInstructions typeInfo ast def astNodes =
    astNodes
        |> List.foldl (astNodeToCodegenNode def ast) ( def.type_.input, [] )
        |> Tuple.second
        |> List.reverse
        |> List.map (nodeToInstruction typeInfo)


astNodeToCodegenNode :
    AST.FunctionDefinition
    -> AST
    -> AST.AstNode
    -> ( List Type, List AstNode )
    -> ( List Type, List AstNode )
astNodeToCodegenNode def ast node ( stack, result ) =
    let
        newNode =
            case node of
                AST.IntLiteral _ val ->
                    IntLiteral val

                AST.Function _ fn type_ ->
                    Word fn type_

                AST.FunctionRef _ fn ->
                    WordRef fn

                AST.Recurse _ ->
                    Word def def.type_

                AST.Cycle _ data ->
                    case Dict.get data.name ast.functions of
                        Just fn ->
                            Word fn data.typeSignature

                        Nothing ->
                            -- can't happen
                            Word def def.type_

                AST.Builtin _ builtin ->
                    Builtin builtin

                AST.ConstructType typeDef ->
                    ConstructType typeDef

                AST.SetMember typeDef memberName memberIndex type_ ->
                    SetMember typeDef memberName memberIndex type_

                AST.GetMember typeDef memberName memberIndex type_ ->
                    GetMember typeDef memberName memberIndex type_

        nodeType =
            case node of
                AST.IntLiteral _ _ ->
                    { input = []
                    , output = [ Type.Int ]
                    }

                AST.Function _ _ type_ ->
                    type_

                AST.FunctionRef _ fn ->
                    { input = []
                    , output = [ Type.FunctionSignature fn.type_ ]
                    }

                AST.Recurse _ ->
                    def.type_

                AST.Cycle _ data ->
                    data.typeSignature

                AST.Builtin _ builtin ->
                    Builtin.functionType builtin

                AST.ConstructType typeDef ->
                    case typeDef.members of
                        StructMembers members ->
                            { input = List.map Tuple.second members
                            , output = [ typeFromTypeDef typeDef.name typeDef.generics ]
                            }

                        UnionMembers members ->
                            -- Cannot happen
                            { input = members
                            , output = [ typeFromTypeDef typeDef.name typeDef.generics ]
                            }

                AST.SetMember typeDef _ _ memberType ->
                    let
                        type_ =
                            typeFromTypeDef typeDef.name typeDef.generics
                    in
                    { input = [ type_, memberType ]
                    , output = [ type_ ]
                    }

                AST.GetMember typeDef _ _ memberType ->
                    let
                        type_ =
                            typeFromTypeDef typeDef.name typeDef.generics
                    in
                    { input = [ type_ ]
                    , output = [ memberType ]
                    }

        typeFromTypeDef typeName gens =
            if List.isEmpty gens then
                Type.Custom typeName

            else
                Type.CustomGeneric typeName (List.map Type.Generic gens)

        stackInScope =
            List.reverse stack
                |> List.take (List.length nodeType.input)
                |> List.reverse

        stackElementsToBox =
            List.map2 Tuple.pair (List.reverse stackInScope) (List.reverse nodeType.input)
                |> List.indexedMap (\i ( l, r ) -> ( i, l, r ))
                |> List.filterMap maybeBox
                |> maybeCons maybeBoxLeadingElement

        maybeBox ( idx, leftType, rightType ) =
            case ( leftType, rightType ) of
                ( _, Type.Union _ members ) ->
                    unionBoxMap members
                        |> List.find (\( t, _ ) -> t == leftType)
                        |> Maybe.map Tuple.second
                        |> Maybe.map (Box idx)

                _ ->
                    Nothing

        maybeBoxLeadingElement =
            case ( List.head stackInScope, isMultiWord newNode, List.head nodeType.input ) of
                ( Just _, True, Just (Type.Union _ _) ) ->
                    -- Already handled by maybePromoteInt
                    Nothing

                ( Just _, True, Just nodeLeadingType ) ->
                    if requiresBoxingInPatternMatch nodeLeadingType then
                        let
                            idx =
                                max 0 (List.length nodeType.input - 1)
                        in
                        Just (Box idx -1)

                    else
                        Nothing

                _ ->
                    Nothing

        isMultiWord possibleMultiWordNode =
            case possibleMultiWordNode of
                Word fn _ ->
                    case fn.implementation of
                        AST.SoloImpl _ ->
                            False

                        AST.MultiImpl _ _ ->
                            True

                _ ->
                    False

        maybeCons maybeBoxElement list =
            case maybeBoxElement of
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
    , newNode :: (stackElementsToBox ++ result)
    )


unionBoxMap : List Type -> List ( Type, Int )
unionBoxMap union =
    let
        helper t ( nextId, mapping ) =
            if requiresBoxingInPatternMatch t then
                ( nextId - 1
                , ( t, nextId ) :: mapping
                )

            else
                ( nextId, mapping )
    in
    List.foldl helper ( -1, [] ) union
        |> Tuple.second


requiresBoxingInPatternMatch : Type -> Bool
requiresBoxingInPatternMatch type_ =
    case type_ of
        Type.Int ->
            True

        Type.Generic _ ->
            True

        _ ->
            False


multiFnToInstructions :
    Dict String TypeInformation
    -> AST
    -> AST.FunctionDefinition
    -> List ( AST.TypeMatch, List AST.AstNode )
    -> List AST.AstNode
    -> Wasm.Instruction
multiFnToInstructions typeInfo ast def whens defaultImpl =
    let
        boxMap =
            def.type_.input
                |> List.head
                |> Maybe.map createBoxMap
                |> Maybe.withDefault []

        createBoxMap t_ =
            case t_ of
                Type.Union _ members ->
                    unionBoxMap members

                _ ->
                    if requiresBoxingInPatternMatch t_ then
                        [ ( t_, -1 ) ]

                    else
                        []

        branches =
            List.foldl buildBranch (Wasm.Batch []) whens

        buildBranch ( type_, nodes ) previousBranch =
            let
                testForInequality =
                    makeInequalityTest type_ 0

                makeInequalityTest t_ localIdx =
                    let
                        (AST.TypeMatch _ typeFromTypeMatch _) =
                            t_

                        maybeBoxId =
                            boxMap
                                |> List.find (\( boxedType, _ ) -> boxedType == typeFromTypeMatch)
                                |> Maybe.map Tuple.second
                    in
                    case ( t_, maybeBoxId ) of
                        ( AST.TypeMatch _ Type.Int conditions, Just boxId ) ->
                            Wasm.Batch
                                [ Wasm.Local_Get localIdx
                                , Wasm.I32_Load -- Load instance id
                                , Wasm.I32_Const boxId
                                , Wasm.I32_NotEq -- Types doesn't match?
                                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                                , conditions
                                    |> List.concatMap (matchingIntTest localIdx)
                                    |> Wasm.Batch
                                , Wasm.I32_Const selfIndex
                                , Wasm.Call BaseModule.unboxFn
                                ]

                        ( AST.TypeMatch _ _ [], Just boxId ) ->
                            Wasm.Batch
                                [ Wasm.Local_Get localIdx
                                , Wasm.I32_Load -- Load instance id
                                , Wasm.I32_Const boxId
                                , Wasm.I32_NotEq -- Types doesn't match?
                                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                                , Wasm.I32_Const selfIndex
                                , Wasm.Call BaseModule.unboxFn
                                ]

                        ( AST.TypeMatch _ (Type.Custom name) conditions, Nothing ) ->
                            whenSetup localIdx name conditions

                        ( AST.TypeMatch _ (Type.CustomGeneric name _) conditions, Nothing ) ->
                            whenSetup localIdx name conditions

                        _ ->
                            Debug.todo <| "Not supported in pattern match: " ++ Debug.toString t_

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

        Word fn _ ->
            Wasm.Call fn.name

        WordRef fn ->
            Wasm.FunctionIndex fn.name

        ConstructType typeDef ->
            case Dict.get typeDef.name typeInfo of
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

        SetMember typeDef _ memberIndex _ ->
            case Dict.get typeDef.name typeInfo of
                Just type_ ->
                    let
                        typeSize =
                            BaseModule.wasmPtrSize + (memberSize * BaseModule.wasmPtrSize)

                        memberSize =
                            List.length type_.members
                    in
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
                    Debug.todo "This cannot happen!"

        GetMember _ _ memberIndex _ ->
            Wasm.Batch
                [ Wasm.Call BaseModule.stackPopFn -- Get instance address
                , Wasm.I32_Const ((memberIndex + 1) * BaseModule.wasmPtrSize) -- Calculate member offset
                , Wasm.I32_Add -- Calculate member address
                , Wasm.I32_Load -- Retrieve member
                , Wasm.Call BaseModule.stackPushFn -- Push member onto stack
                ]

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
                    Wasm.Call BaseModule.callQuoteFn

        Box stackPos id ->
            Wasm.Batch
                [ Wasm.I32_Const stackPos
                , Wasm.I32_Const id
                , Wasm.Call BaseModule.boxFn
                ]
