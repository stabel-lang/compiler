module Stabel.Codegen exposing (run)

import Dict
import List.Extra as List
import Set exposing (Set)
import Stabel.Codegen.BaseModule as BaseModule
import Stabel.Codegen.IdAssigner as IdAssigner exposing (IdAssigner)
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.Type as Type exposing (Type)
import Stabel.Qualifier exposing (TypeDefinitionMembers(..))
import Stabel.TypeChecker as AST exposing (AST)
import Stabel.Wasm as Wasm


type alias Context =
    { functionIdAssignment : IdAssigner
    , typeIdAssignment : IdAssigner
    , inlineFunctionNames : List String
    }


emptyContext : Context
emptyContext =
    { functionIdAssignment = IdAssigner.empty BaseModule.firstAvailableFunctionId
    , typeIdAssignment = IdAssigner.empty 0
    , inlineFunctionNames = []
    }


idForFunction : String -> Context -> ( Int, Context )
idForFunction funcName context =
    let
        ( id, assign ) =
            IdAssigner.assignId funcName context.functionIdAssignment
    in
    ( id
    , { context | functionIdAssignment = assign }
    )


idForType : String -> Context -> ( Int, Context )
idForType typeName context =
    let
        ( id, assign ) =
            IdAssigner.assignId typeName context.typeIdAssignment
    in
    ( id
    , { context | typeIdAssignment = assign }
    )


indexOf : String -> List String -> Maybe Int
indexOf value list =
    indexOfHelper 0 value list


indexOfHelper : Int -> String -> List String -> Maybe Int
indexOfHelper idx value list =
    case list of
        [] ->
            Nothing

        element :: rest ->
            if element == value then
                Just idx

            else
                indexOfHelper (idx + 1) value rest



-- Codegen


type AstNode
    = IntLiteral Int
    | ArrayLiteral (List AstNode)
    | Function Int String -- id name
    | FunctionRef Int String -- id name
    | ConstructType Int Int -- id memberQty
    | SetMember Int Int -- offset memberQty
    | GetMember Int -- offset
    | Builtin Builtin
    | Box Int Int -- stackIdx typeId


run : Set String -> AST -> Wasm.Module
run exportedFunctions ast =
    let
        inlineFunctionNames =
            Set.toList ast.referencableFunctions

        ( wasmFunctions, context ) =
            Dict.foldl
                (\_ fn acc -> toWasmFuncDef fn acc)
                ( [], { emptyContext | inlineFunctionNames = inlineFunctionNames } )
                ast.functions

        inlineFunctionRefs =
            List.filterMap
                (\name -> Dict.get name context.functionIdAssignment.cache)
                inlineFunctionNames

        exportedFunctionRefs =
            Set.foldr
                (\name acc ->
                    context.functionIdAssignment.cache
                        |> Dict.get name
                        |> Maybe.map (\id -> ( name, id ))
                        |> Maybe.map (\res -> res :: acc)
                        |> Maybe.withDefault acc
                )
                []
                exportedFunctions
    in
    wasmFunctions
        |> List.foldl Wasm.withFunction BaseModule.baseModule
        |> Wasm.withReferencables inlineFunctionRefs
        |> Wasm.withExports exportedFunctionRefs


toWasmFuncDef :
    AST.FunctionDefinition
    -> ( List Wasm.FunctionDefinition, Context )
    -> ( List Wasm.FunctionDefinition, Context )
toWasmFuncDef def ( wasmFuncs, context ) =
    let
        ( defId, idContext ) =
            idForFunction def.name context

        ( wasmImplementation, updatedContext ) =
            case def.implementation of
                AST.SoloImpl impl ->
                    astNodesToInstructions idContext def impl

                AST.MultiImpl whens defaultImpl ->
                    multiFnToInstructions idContext def whens defaultImpl

        numberOfLocals =
            List.filterMap Wasm.maximumLocalIndex wasmImplementation
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
    in
    ( { id = defId
      , name = def.name
      , args = []
      , results = []
      , locals = List.repeat numberOfLocals Wasm.Int32
      , instructions = wasmImplementation
      }
        :: wasmFuncs
    , updatedContext
    )


astNodesToInstructions :
    Context
    -> AST.FunctionDefinition
    -> List AST.AstNode
    -> ( List Wasm.Instruction, Context )
astNodesToInstructions context def astNodes =
    let
        ( _, codeGenNodes, updatedContext ) =
            List.foldl
                (astNodeToCodegenNode def)
                ( def.type_.input, [], context )
                astNodes
    in
    ( codeGenNodes
        |> List.reverse
        |> List.map (nodeToInstruction updatedContext)
    , updatedContext
    )


astNodeToCodegenNode :
    AST.FunctionDefinition
    -> AST.AstNode
    -> ( List Type, List AstNode, Context )
    -> ( List Type, List AstNode, Context )
astNodeToCodegenNode def node ( stack, result, context ) =
    let
        ( newNode, updatedContext ) =
            case node of
                AST.IntLiteral _ val ->
                    ( IntLiteral val
                    , context
                    )

                AST.ArrayLiteral _ nodes _ ->
                    let
                        ( _, codeGenNodes, nextContext ) =
                            List.foldl
                                (astNodeToCodegenNode def)
                                ( [], [], context )
                                nodes
                    in
                    ( ArrayLiteral (List.reverse codeGenNodes)
                    , nextContext
                    )

                AST.Function _ fn _ ->
                    let
                        ( fnId, newContext ) =
                            idForFunction fn.name context
                    in
                    ( Function fnId fn.name
                    , newContext
                    )

                AST.FunctionRef _ fn ->
                    let
                        ( fnId, newContext ) =
                            idForFunction fn.name context
                    in
                    ( FunctionRef fnId fn.name
                    , newContext
                    )

                AST.Recurse _ ->
                    let
                        ( fnId, newContext ) =
                            idForFunction def.name context
                    in
                    ( Function fnId def.name
                    , newContext
                    )

                AST.Cycle _ data ->
                    let
                        ( fnId, newContext ) =
                            idForFunction data.name context
                    in
                    ( Function fnId data.name
                    , newContext
                    )

                AST.Builtin _ builtin ->
                    ( Builtin builtin
                    , context
                    )

                AST.ConstructType typeDef ->
                    let
                        ( typeId, newContext ) =
                            idForType typeDef.name context
                    in
                    ( ConstructType typeId (memberSize typeDef)
                    , newContext
                    )

                AST.SetMember typeDef _ memberIndex _ ->
                    ( SetMember memberIndex (memberSize typeDef)
                    , context
                    )

                AST.GetMember _ _ memberIndex _ ->
                    ( GetMember memberIndex
                    , context
                    )

        nodeType =
            case node of
                AST.IntLiteral _ _ ->
                    { input = []
                    , output = [ Type.Int ]
                    }

                AST.ArrayLiteral _ _ t ->
                    { input = []
                    , output = [ t ]
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
            case ( List.head stackInScope, isMultiFunction node, List.head nodeType.input ) of
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

        isMultiFunction possibleMultiFunctionNode =
            case possibleMultiFunctionNode of
                AST.Function _ fn _ ->
                    case fn.implementation of
                        AST.SoloImpl _ ->
                            False

                        AST.MultiImpl _ _ ->
                            True

                AST.Cycle _ data ->
                    data.isMultiFunction

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
    , updatedContext
    )


memberSize : AST.TypeDefinition -> Int
memberSize def =
    case def.members of
        StructMembers members ->
            List.length members

        UnionMembers members ->
            List.length members


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
    Context
    -> AST.FunctionDefinition
    -> List ( AST.TypeMatch, List AST.AstNode )
    -> List AST.AstNode
    -> ( List Wasm.Instruction, Context )
multiFnToInstructions context def whens defaultImpl =
    let
        ( branches, updatedContext ) =
            List.foldl
                (buildMultiFnBranch def boxMap selfIndex)
                ( Wasm.Batch [], context )
                whens

        boxMap =
            def.type_.input
                |> List.head
                |> Maybe.map createBoxMap
                |> Maybe.withDefault []

        selfIndex =
            max 0 (List.length def.type_.input - 1)

        ( implementation, finalContext ) =
            astNodesToInstructions updatedContext def defaultImpl
    in
    ( [ Wasm.I32_Const selfIndex
      , BaseModule.callStackGetElementFn
      , Wasm.Local_Set 0 -- store instance id in local
      , branches
      , Wasm.Batch implementation
      ]
    , finalContext
    )


createBoxMap : Type -> List ( Type, Int )
createBoxMap t_ =
    case t_ of
        Type.Union _ members ->
            unionBoxMap members

        _ ->
            if requiresBoxingInPatternMatch t_ then
                [ ( t_, -1 ) ]

            else
                []


buildMultiFnBranch :
    AST.FunctionDefinition
    -> List ( Type, Int )
    -> Int
    -> ( AST.TypeMatch, List AST.AstNode )
    -> ( Wasm.Instruction, Context )
    -> ( Wasm.Instruction, Context )
buildMultiFnBranch def boxMap selfIndex ( type_, nodes ) ( previousBranch, context ) =
    let
        ( instructions, updatedContext ) =
            astNodesToInstructions context def nodes

        ( inequalityTestImpl, finalContext ) =
            makeInequalityTest boxMap selfIndex type_ 0 updatedContext
    in
    ( Wasm.Block
        [ previousBranch
        , inequalityTestImpl
        , Wasm.Batch instructions
        , Wasm.Return
        ]
    , finalContext
    )


makeInequalityTest :
    List ( Type, Int )
    -> Int
    -> AST.TypeMatch
    -> Int
    -> Context
    -> ( Wasm.Instruction, Context )
makeInequalityTest boxMap selfIndex ((AST.TypeMatch _ typeFromTypeMatch _) as t_) localIdx context =
    let
        maybeBoxId =
            boxMap
                |> List.find (\( boxedType, _ ) -> boxedType == typeFromTypeMatch)
                |> Maybe.map Tuple.second
    in
    case ( t_, maybeBoxId ) of
        ( AST.TypeMatch _ Type.Int conditions, Just boxId ) ->
            ( Wasm.Batch
                [ Wasm.Local_Get localIdx
                , Wasm.I32_Load -- Load instance id
                , Wasm.I32_Const boxId
                , Wasm.I32_NotEq -- Types doesn't match?
                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                , conditions
                    |> List.concatMap (matchingIntTest localIdx)
                    |> Wasm.Batch
                , Wasm.I32_Const selfIndex
                , BaseModule.callUnboxFn
                ]
            , context
            )

        ( AST.TypeMatch _ _ [], Just boxId ) ->
            ( Wasm.Batch
                [ Wasm.Local_Get localIdx
                , Wasm.I32_Load -- Load instance id
                , Wasm.I32_Const boxId
                , Wasm.I32_NotEq -- Types doesn't match?
                , Wasm.BreakIf 0 -- Move to next branch if above test is true
                , Wasm.I32_Const selfIndex
                , BaseModule.callUnboxFn
                ]
            , context
            )

        ( AST.TypeMatch _ (Type.Custom name) conditions, Nothing ) ->
            matchingStructTest boxMap selfIndex context localIdx name conditions

        ( AST.TypeMatch _ (Type.CustomGeneric name _) conditions, Nothing ) ->
            matchingStructTest boxMap selfIndex context localIdx name conditions

        _ ->
            -- Type not supported in pattern match
            -- TODO: TypeMatch should maybe change to only support types
            -- which are supported in pattern matches
            ( Wasm.Unreachable, context )


matchingIntTest : Int -> ( String, AST.TypeMatchValue ) -> List Wasm.Instruction
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


matchingStructTest :
    List ( Type, Int )
    -> Int
    -> Context
    -> Int
    -> String
    -> List ( String, AST.TypeMatchValue )
    -> ( Wasm.Instruction, Context )
matchingStructTest boxMap selfIndex context localIdx typeName conditions =
    let
        ( typeId, updatedContext ) =
            idForType typeName context

        ( conditionTestImpls, finalContext ) =
            List.foldl
                (matchingConditionTest boxMap selfIndex localIdx)
                ( [], updatedContext )
                conditions
    in
    ( Wasm.Batch
        [ Wasm.Local_Get localIdx
        , Wasm.I32_Load -- Load instance id
        , Wasm.I32_Const typeId
        , Wasm.I32_NotEq -- Types doesn't match?
        , Wasm.BreakIf 0 -- Move to next branch if above test is true
        , conditionTestImpls
            |> List.concat
            |> Wasm.Batch
        ]
    , finalContext
    )


matchingConditionTest :
    List ( Type, Int )
    -> Int
    -> Int
    -> ( String, AST.TypeMatchValue )
    -> ( List (List Wasm.Instruction), Context )
    -> ( List (List Wasm.Instruction), Context )
matchingConditionTest boxMap selfIndex localIdx ( fieldName, value ) ( result, context ) =
    let
        getterName =
            fieldName ++ ">"

        ( getterId, idContext ) =
            idForFunction getterName context

        callGetter =
            Wasm.Call getterId getterName
    in
    case value of
        AST.LiteralInt num ->
            ( [ Wasm.Local_Get localIdx
              , BaseModule.callStackPushFn
              , callGetter
              , BaseModule.callStackPopFn
              , Wasm.I32_Const num
              , Wasm.I32_NotEq -- not same number?
              , Wasm.BreakIf 0 -- move to next branch
              ]
                :: result
            , idContext
            )

        AST.LiteralType typ_ ->
            case typ_ of
                Type.Custom typeName ->
                    let
                        ( typeId, updatedContext ) =
                            idForType typeName idContext
                    in
                    ( [ Wasm.Local_Get localIdx
                      , BaseModule.callStackPushFn
                      , callGetter
                      , BaseModule.callStackPopFn
                      , Wasm.I32_Load -- get type id
                      , Wasm.I32_Const typeId
                      , Wasm.I32_NotEq -- not same type?
                      , Wasm.BreakIf 0 -- move to next branch
                      ]
                        :: result
                    , updatedContext
                    )

                _ ->
                    ( [ Wasm.Unreachable ] :: result
                    , context
                    )

        AST.RecursiveMatch match ->
            let
                nextLocalIdx =
                    localIdx + 1

                ( inequalityTestImpl, updatedContext ) =
                    makeInequalityTest boxMap selfIndex match nextLocalIdx idContext
            in
            ( [ Wasm.Local_Get localIdx
              , BaseModule.callStackPushFn
              , callGetter
              , BaseModule.callStackPopFn
              , Wasm.Local_Set nextLocalIdx
              , inequalityTestImpl
              ]
                :: result
            , updatedContext
            )


nodeToInstruction : Context -> AstNode -> Wasm.Instruction
nodeToInstruction context node =
    case node of
        IntLiteral value ->
            Wasm.Batch
                [ Wasm.I32_Const value
                , BaseModule.callStackPushFn
                ]

        ArrayLiteral nodes ->
            Wasm.Batch <|
                BaseModule.callArrayEmptyFn
                    :: List.concatMap (\n -> [ nodeToInstruction context n, BaseModule.callArrayPushFn ]) nodes

        Function id name ->
            Wasm.Call id name

        FunctionRef _ name ->
            let
                indexOfId =
                    indexOf name context.inlineFunctionNames
                        |> Maybe.withDefault 0
            in
            Wasm.Batch
                [ Wasm.Commented
                    (name ++ "ref")
                    (Wasm.I32_Const indexOfId)
                , BaseModule.callStackPushFn
                ]

        ConstructType typeId members ->
            let
                typeSize =
                    BaseModule.wasmPtrSize + (members * BaseModule.wasmPtrSize)
            in
            Wasm.Batch
                [ Wasm.I32_Const typeSize
                , BaseModule.callAllocFn
                , Wasm.Local_Tee 0
                , Wasm.I32_Const typeId
                , Wasm.I32_Store
                , Wasm.I32_Const members
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
                        , BaseModule.callStackPopFn
                        , Wasm.I32_Store
                        , Wasm.Local_Get 1
                        , Wasm.I32_Const 1
                        , Wasm.I32_Sub
                        , Wasm.Local_Set 1
                        , Wasm.Break 0
                        ]
                    ]
                , Wasm.Local_Get 0
                , BaseModule.callStackPushFn
                ]

        SetMember memberIndex members ->
            let
                typeSize =
                    BaseModule.wasmPtrSize + (members * BaseModule.wasmPtrSize)
            in
            Wasm.Batch
                [ BaseModule.callSwapFn -- Instance should now be at top of stack
                , BaseModule.callStackPopFn
                , Wasm.I32_Const typeSize
                , BaseModule.callCopyStructFn -- Return copy of instance
                , Wasm.Local_Tee 0
                , Wasm.I32_Const ((memberIndex + 1) * BaseModule.wasmPtrSize) -- Calculate member offset
                , Wasm.I32_Add -- Calculate member address
                , BaseModule.callStackPopFn -- Retrieve new value
                , Wasm.I32_Store
                , Wasm.Local_Get 0 -- Return instance
                , BaseModule.callStackPushFn
                ]

        GetMember memberIndex ->
            Wasm.Batch
                [ BaseModule.callStackPopFn -- Get instance address
                , Wasm.I32_Const ((memberIndex + 1) * BaseModule.wasmPtrSize) -- Calculate member offset
                , Wasm.I32_Add -- Calculate member address
                , Wasm.I32_Load -- Retrieve member
                , BaseModule.callStackPushFn -- Push member onto stack
                ]

        Builtin builtin ->
            case builtin of
                Builtin.Plus ->
                    BaseModule.callAddIntFn

                Builtin.Minus ->
                    BaseModule.callSubIntFn

                Builtin.Multiply ->
                    BaseModule.callMulIntFn

                Builtin.Divide ->
                    BaseModule.callDivIntFn

                Builtin.Equal ->
                    BaseModule.callEqIntFn

                Builtin.StackDuplicate ->
                    BaseModule.callDupFn

                Builtin.StackDrop ->
                    BaseModule.callDropFn

                Builtin.StackSwap ->
                    BaseModule.callSwapFn

                Builtin.StackRightRotate ->
                    BaseModule.callRotFn

                Builtin.StackLeftRotate ->
                    BaseModule.callLeftRotFn

                Builtin.Apply ->
                    BaseModule.callExecInlineFn

                Builtin.ArrayEmpty ->
                    BaseModule.callArrayEmptyFn

                Builtin.ArrayLength ->
                    BaseModule.callArrayLengthFn

                Builtin.ArrayPush ->
                    BaseModule.callArrayPushFn

                Builtin.ArrayGet ->
                    BaseModule.callArrayGetFn

                Builtin.ArraySet ->
                    BaseModule.callArraySetFn

        Box stackPos id ->
            Wasm.Batch
                [ Wasm.I32_Const stackPos
                , Wasm.I32_Const id
                , BaseModule.callBoxFn
                ]
