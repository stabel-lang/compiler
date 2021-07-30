module Wasm exposing (..)

import List.Extra as List


type Module
    = Module
        { typeSignatures : List TypeSignature
        , functions : List Function
        , nextFunctionIndex : Int
        , quotables : List Int
        , imports : List Import
        , exports : List Int
        , start : Maybe Int
        }


type alias Function =
    { name : String
    , typeSignatureIndex : Int
    , locals : List Type
    , instructions : List Instruction
    }


type alias TypeSignature =
    { inputs : List Type
    , outputs : List Type
    }


type alias Import =
    { moduleName : String
    , entityName : String
    , type_ : ModuleType
    }


type ModuleType
    = Memory Int (Maybe Int)


moduleTypeToString : ModuleType -> String
moduleTypeToString moduleType =
    case moduleType of
        Memory lower Nothing ->
            "(memory " ++ String.fromInt lower ++ ")"

        Memory lower (Just upper) ->
            "(memory " ++ String.fromInt lower ++ " " ++ String.fromInt upper ++ ")"


type Type
    = Int32


typeToString : Type -> String
typeToString type_ =
    case type_ of
        Int32 ->
            "i32"


type Instruction
    = NoOp
    | Batch (List Instruction)
    | Block (List Instruction)
    | Loop (List Instruction)
    | Break Int
    | BreakIf Int
    | Return
    | Call String
    | CallIndirect
    | FunctionIndex String -- Not actual WASM, do re-evaluate
    | Local_Get Int
    | Local_Set Int
    | Local_Tee Int
    | I32_Const Int
    | I32_Add
    | I32_Sub
    | I32_Mul
    | I32_Div
    | I32_Eq
    | I32_NotEq
    | I32_EqZero
    | I32_Store
    | I32_Load
    | Drop
    | Unreachable
    | Commented String Instruction


maximumLocalIndex : Instruction -> Maybe Int
maximumLocalIndex ins =
    case ins of
        Batch insList ->
            List.filterMap maximumLocalIndex insList
                |> List.maximum

        Block insList ->
            List.filterMap maximumLocalIndex insList
                |> List.maximum

        Loop insList ->
            List.filterMap maximumLocalIndex insList
                |> List.maximum

        Local_Get idx ->
            Just idx

        Local_Set idx ->
            Just idx

        Local_Tee idx ->
            Just idx

        _ ->
            Nothing


initModule : Module
initModule =
    Module
        { typeSignatures = []
        , nextFunctionIndex = 0
        , functions = []
        , quotables = []
        , imports = []
        , exports = []
        , start = Nothing
        }


type alias FunctionDef =
    { name : String
    , exported : Bool
    , isIndirectlyCalled : Bool
    , args : List Type
    , results : List Type
    , locals : List Type
    , instructions : List Instruction
    }


withFunction : FunctionDef -> Module -> Module
withFunction funcDef (Module module_) =
    let
        typeSignature =
            { inputs = funcDef.args
            , outputs = funcDef.results
            }

        ( tsIndex, updatedModule ) =
            case List.elemIndex typeSignature module_.typeSignatures of
                Just idx ->
                    ( idx, module_ )

                Nothing ->
                    ( List.length module_.typeSignatures
                    , { module_
                        | typeSignatures = module_.typeSignatures ++ [ typeSignature ]
                      }
                    )

        newFunction =
            { name = funcDef.name
            , typeSignatureIndex = tsIndex
            , locals = funcDef.locals
            , instructions = funcDef.instructions
            }
    in
    Module <|
        { updatedModule
            | functions = updatedModule.functions ++ [ newFunction ]
            , nextFunctionIndex = updatedModule.nextFunctionIndex + 1
            , quotables =
                if funcDef.isIndirectlyCalled then
                    updatedModule.quotables ++ [ updatedModule.nextFunctionIndex ]

                else
                    updatedModule.quotables
            , exports =
                if funcDef.exported then
                    updatedModule.exports ++ [ updatedModule.nextFunctionIndex ]

                else
                    updatedModule.exports
        }


withStartFunction : FunctionDef -> Module -> Module
withStartFunction funcDef module_ =
    let
        (Module moduleWithFunction) =
            withFunction funcDef module_

        startIdx =
            List.length moduleWithFunction.functions - 1
    in
    Module <|
        { moduleWithFunction | start = Just startIdx }


withImport : String -> String -> ModuleType -> Module -> Module
withImport importModule entityName typeToImport (Module module_) =
    Module
        { module_
            | imports =
                module_.imports
                    ++ [ { moduleName = importModule
                         , entityName = entityName
                         , type_ = typeToImport
                         }
                       ]
        }


toString : Module -> String
toString ((Module module_) as fullModule) =
    [ Str "(module"
    , List.concatMap formatImports module_.imports
        |> Indent
    , List.concatMap formatTypeSignature module_.typeSignatures
        |> Indent
    , formatTable fullModule
        |> Indent
    , List.concatMap (formatFunction fullModule) module_.functions
        |> Indent
    , List.concatMap (formatExport fullModule) module_.exports
        |> Indent
    , formatStartFunction module_.start
    , Str ")"
    ]
        |> List.map format
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


formatImports : Import -> List FormatHint
formatImports importType =
    [ Str <| "(import \"" ++ importType.moduleName ++ "\" \"" ++ importType.entityName ++ "\" " ++ moduleTypeToString importType.type_ ++ ")" ]


formatTypeSignature : TypeSignature -> List FormatHint
formatTypeSignature typeSignature =
    let
        inputs =
            if List.isEmpty typeSignature.inputs then
                ""

            else
                "(param " ++ (String.join " " <| List.map typeToString typeSignature.inputs) ++ ")"

        results =
            if List.isEmpty typeSignature.outputs then
                ""

            else
                "(result " ++ (String.join " " <| List.map typeToString typeSignature.outputs) ++ ")"

        formattedSignature =
            [ "(type"
            , "(func"
            , inputs
            , results
            , "))"
            ]
                |> List.filter (not << String.isEmpty)
                |> String.join " "
    in
    [ Str formattedSignature ]


formatTable : Module -> List FormatHint
formatTable (Module module_) =
    let
        tableDef =
            String.join " "
                [ "(table"
                , String.fromInt (List.length module_.quotables)
                , "funcref)"
                ]

        elemDef =
            String.join " "
                [ "(elem"
                , "(i32.const 0)"
                , String.join " " <|
                    List.map (\funcIdx -> String.fromInt funcIdx) module_.quotables
                , ")"
                ]
    in
    [ Str tableDef
    , Str elemDef
    ]


formatFunction : Module -> Function -> List FormatHint
formatFunction module_ function =
    let
        locals =
            if List.isEmpty function.locals then
                ""

            else
                "(local " ++ (String.join " " <| List.map typeToString function.locals) ++ ")"

        fullFuncDef =
            String.join " "
                [ "(func"
                , "$" ++ String.replace "," "__COMMA__" function.name
                , "(type " ++ String.fromInt function.typeSignatureIndex ++ ")"
                , locals
                ]
    in
    [ Str fullFuncDef
    , Indent <| List.map (formatInstruction module_) function.instructions
    , Str ")"
    ]


formatInstruction : Module -> Instruction -> FormatHint
formatInstruction ((Module module_) as fullModule) ins =
    case ins of
        NoOp ->
            Str "nop"

        Batch insList ->
            BatchFormat <| List.map (formatInstruction fullModule) insList

        Block insList ->
            BatchFormat
                [ Str "(block"
                , Indent <| List.map (formatInstruction fullModule) insList
                , Str ")"
                ]

        Loop insList ->
            BatchFormat
                [ Str "(loop"
                , Indent <| List.map (formatInstruction fullModule) insList
                , Str ")"
                ]

        Break num ->
            Str <| "(br " ++ String.fromInt num ++ ")"

        BreakIf num ->
            Str <| "(br_if " ++ String.fromInt num ++ ")"

        Return ->
            Str "return"

        Call word ->
            case List.findIndex (\f -> f.name == word) module_.functions of
                Just idx ->
                    Str <| "(call " ++ String.fromInt idx ++ ") ;; $" ++ word

                Nothing ->
                    Debug.todo "Did not expect this"

        CallIndirect ->
            Str "call_indirect"

        FunctionIndex word ->
            case List.findIndex (\f -> f.name == word) module_.functions of
                Just idx ->
                    case List.findIndex ((==) idx) module_.quotables of
                        Just quoteIdx ->
                            BatchFormat
                                [ Str <| "(i32.const " ++ String.fromInt quoteIdx ++ ") ;; $" ++ word
                                , Str "(call $__stack_push)" -- TODO: WASM module should have no knowledge of runtime
                                ]

                        Nothing ->
                            Debug.todo "Did not expect this"

                Nothing ->
                    Debug.todo "Did not expect this"

        Local_Get idx ->
            Str <| "(local.get " ++ String.fromInt idx ++ ")"

        Local_Set idx ->
            Str <| "(local.set " ++ String.fromInt idx ++ ")"

        Local_Tee idx ->
            Str <| "(local.tee " ++ String.fromInt idx ++ ")"

        I32_Const num ->
            Str <| "(i32.const " ++ String.fromInt num ++ ")"

        I32_Add ->
            Str "i32.add"

        I32_Sub ->
            Str "i32.sub"

        I32_Mul ->
            Str "i32.mul"

        I32_Div ->
            Str "i32.div_s"

        I32_Eq ->
            Str "i32.eq"

        I32_NotEq ->
            Str "i32.ne"

        I32_EqZero ->
            Str "i32.eqz"

        I32_Store ->
            Str "i32.store"

        I32_Load ->
            Str "i32.load"

        Drop ->
            Str "drop"

        Unreachable ->
            Str "unreachable"

        Commented comment inst ->
            case formatInstruction fullModule inst of
                Str val ->
                    Str <| val ++ ";; " ++ comment

                BatchFormat batch ->
                    BatchFormat <| (Str <| ";; " ++ comment) :: batch

                Indent batch ->
                    Indent <| (Str <| ";; " ++ comment) :: batch


formatExport : Module -> Int -> List FormatHint
formatExport (Module module_) idx =
    case List.getAt idx module_.functions of
        Just func ->
            [ Str <| "(export \"" ++ func.name ++ "\" (func " ++ String.fromInt idx ++ "))" ]

        Nothing ->
            Debug.todo "Did not expect this..."


formatStartFunction : Maybe Int -> FormatHint
formatStartFunction maybeStartFunction =
    case maybeStartFunction of
        Nothing ->
            Str ""

        Just idx ->
            Indent [ Str <| "(start " ++ String.fromInt idx ++ ")" ]


type FormatHint
    = Str String
    | Indent (List FormatHint)
    | BatchFormat (List FormatHint)


format : FormatHint -> String
format hint =
    formatHelper 0 hint
        |> Maybe.withDefault ""


formatHelper : Int -> FormatHint -> Maybe String
formatHelper indentation hint =
    case hint of
        Str "" ->
            Nothing

        Str value ->
            applyIndentation indentation value
                |> Just

        Indent [] ->
            Nothing

        Indent strs ->
            strs
                |> List.filterMap (formatHelper (indentation + 2))
                |> String.join "\n"
                |> Just

        BatchFormat [] ->
            Nothing

        BatchFormat hints ->
            hints
                |> List.filterMap (formatHelper indentation)
                |> String.join "\n"
                |> Just


applyIndentation : Int -> String -> String
applyIndentation indent str =
    String.repeat indent " " ++ str
