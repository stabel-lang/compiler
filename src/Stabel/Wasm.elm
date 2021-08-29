module Stabel.Wasm exposing
    ( FunctionDefinition
    , Instruction(..)
    , Module
    , ModuleType(..)
    , Type(..)
    , initModule
    , maximumLocalIndex
    , toString
    , withExports
    , withFunction
    , withImport
    , withReferencables
    , withStartFunction
    )

import List.Extra as List


type Module
    = Module
        { typeSignatures : List TypeSignature
        , functions : List Function
        , referencableFunctions : List Int
        , imports : List Import
        , exports : List ( String, Int )
        , start : Maybe Int
        }


type alias Function =
    { id : Int
    , name : String
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
typeToString _ =
    "i32"


type Instruction
    = Batch (List Instruction)
    | Block (List Instruction)
    | Loop (List Instruction)
    | Break Int
    | BreakIf Int
    | If (List Instruction) (List Instruction)
    | Return
    | Call Int String
    | CallIndirect
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
    | I32_LT
    | I32_GTE
    | Drop
    | Unreachable
    | Commented String Instruction
    | Memory_Copy


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
        , functions = []
        , referencableFunctions = []
        , imports = []
        , exports = []
        , start = Nothing
        }


type alias FunctionDefinition =
    { id : Int
    , name : String
    , args : List Type
    , results : List Type
    , locals : List Type
    , instructions : List Instruction
    }


withFunction : FunctionDefinition -> Module -> Module
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
            { id = funcDef.id
            , name = funcDef.name
            , typeSignatureIndex = tsIndex
            , locals = funcDef.locals
            , instructions = funcDef.instructions
            }
    in
    Module <|
        { updatedModule | functions = newFunction :: updatedModule.functions }


withStartFunction : Int -> Module -> Module
withStartFunction startIdx (Module fields) =
    Module <|
        { fields | start = Just startIdx }


withImport : String -> String -> ModuleType -> Module -> Module
withImport importModule entityName typeToImport (Module module_) =
    Module
        { module_
            | imports =
                { moduleName = importModule
                , entityName = entityName
                , type_ = typeToImport
                }
                    :: module_.imports
        }


withReferencables : List Int -> Module -> Module
withReferencables references (Module module_) =
    Module <|
        { module_ | referencableFunctions = references }


withExports : List ( String, Int ) -> Module -> Module
withExports exports (Module module_) =
    Module <|
        { module_ | exports = exports }


toString : Module -> String
toString (Module module_) =
    [ Str "(module"
    , List.map formatImport module_.imports
        |> Indent
    , List.concatMap formatTypeSignature module_.typeSignatures
        |> Indent
    , formatTable module_.referencableFunctions
        |> Indent
    , List.concatMap formatFunction (List.sortBy .id module_.functions)
        |> Indent
    , List.map formatExport module_.exports
        |> Indent
    , formatStartFunction module_.start
    , Str ")"
    ]
        |> List.map format
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


formatImport : Import -> FormatHint
formatImport importType =
    Str <|
        "(import \""
            ++ importType.moduleName
            ++ "\" \""
            ++ importType.entityName
            ++ "\" "
            ++ moduleTypeToString importType.type_
            ++ ")"


formatTypeSignature : TypeSignature -> List FormatHint
formatTypeSignature typeSignature =
    let
        inputs =
            if List.isEmpty typeSignature.inputs then
                ""

            else
                "(param "
                    ++ (String.join " " <| List.map typeToString typeSignature.inputs)
                    ++ ")"

        results =
            if List.isEmpty typeSignature.outputs then
                ""

            else
                "(result "
                    ++ (String.join " " <| List.map typeToString typeSignature.outputs)
                    ++ ")"

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


formatTable : List Int -> List FormatHint
formatTable references =
    let
        tableDef =
            String.join " "
                [ "(table"
                , String.fromInt (List.length references)
                , "funcref)"
                ]

        elemDef =
            String.join " "
                [ "(elem"
                , "(i32.const 0)"
                , String.join " " <|
                    List.map String.fromInt references
                , ")"
                ]
    in
    [ Str tableDef
    , Str elemDef
    ]


formatFunction : Function -> List FormatHint
formatFunction function =
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
    , Indent <| List.map formatInstruction function.instructions
    , Str ")"
    ]


formatInstruction : Instruction -> FormatHint
formatInstruction ins =
    case ins of
        Batch insList ->
            BatchFormat <| List.map formatInstruction insList

        Block insList ->
            BatchFormat
                [ Str "(block"
                , Indent <| List.map formatInstruction insList
                , Str ")"
                ]

        Loop insList ->
            BatchFormat
                [ Str "(loop"
                , Indent <| List.map formatInstruction insList
                , Str ")"
                ]

        Break num ->
            Str <| "(br " ++ String.fromInt num ++ ")"

        BreakIf num ->
            Str <| "(br_if " ++ String.fromInt num ++ ")"

        If thenIns elseIns ->
            if List.isEmpty elseIns then
                BatchFormat
                    [ Str "(if (then"
                    , Indent <| List.map formatInstruction thenIns
                    , Str "))"
                    ]

            else
                BatchFormat
                    [ Str "(if (then"
                    , Indent <| List.map formatInstruction thenIns
                    , Str ")"
                    , Str "(else"
                    , Indent <| List.map formatInstruction elseIns
                    , Str "))"
                    ]

        Return ->
            Str "return"

        Call id fnName ->
            Str <| "(call " ++ String.fromInt id ++ ") ;; $" ++ fnName

        CallIndirect ->
            Str "call_indirect"

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

        I32_LT ->
            Str "i32.lt_s"

        I32_GTE ->
            Str "i32.gte_s"

        Drop ->
            Str "drop"

        Unreachable ->
            Str "unreachable"

        Commented comment inst ->
            case formatInstruction inst of
                Str val ->
                    Str <| val ++ ";; " ++ comment

                BatchFormat batch ->
                    BatchFormat <| (Str <| ";; " ++ comment) :: batch

                Indent batch ->
                    Indent <| (Str <| ";; " ++ comment) :: batch

        Memory_Copy ->
            Str "memory.copy"


formatExport : ( String, Int ) -> FormatHint
formatExport ( functionName, index ) =
    Str <|
        "(export \""
            ++ functionName
            ++ "\" (func "
            ++ String.fromInt index
            ++ "))"


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
