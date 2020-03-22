module Wasm exposing (..)

import List.Extra as List


type Module
    = Module
        { typeSignatures : List TypeSignature
        , functions : List Function
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
    | Call String
    | Local_Get Int
    | Local_Set Int
    | Local_Tee Int
    | I32_Const Int
    | I32_Add
    | I32_Sub
    | I32_Eq
    | I32_Store
    | I32_Load


instructionToString : Module -> Instruction -> String
instructionToString ((Module module_) as fullModule) ins =
    case ins of
        NoOp ->
            "nop"

        Batch insList ->
            insList
                |> List.map (instructionToString fullModule)
                |> String.join " "

        Call word ->
            case List.findIndex (\f -> f.name == word) module_.functions of
                Just idx ->
                    "(call " ++ String.fromInt idx ++ ") ;; $" ++ word

                Nothing ->
                    Debug.todo "Did not expect this"

        Local_Get idx ->
            "(local.get " ++ String.fromInt idx ++ ")"

        Local_Set idx ->
            "(local.set " ++ String.fromInt idx ++ ")"

        Local_Tee idx ->
            "(local.tee " ++ String.fromInt idx ++ ")"

        I32_Const num ->
            "(i32.const " ++ String.fromInt num ++ ")"

        I32_Add ->
            "i32.add"

        I32_Sub ->
            "i32.sub"

        I32_Eq ->
            "i32.eq"

        I32_Store ->
            "i32.store"

        I32_Load ->
            "i32.load"


initModule : Module
initModule =
    Module
        { typeSignatures = []
        , functions = []
        , imports = []
        , exports = []
        , start = Nothing
        }


type alias FunctionDef =
    { name : String
    , exported : Bool
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
            , exports =
                if funcDef.exported then
                    updatedModule.exports ++ [ List.length updatedModule.functions ]

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
                , "$" ++ function.name
                , "(type " ++ String.fromInt function.typeSignatureIndex ++ ")"
                , locals
                ]
    in
    [ Str fullFuncDef
    , Indent <| List.map (Str << instructionToString module_) function.instructions
    , Str ")"
    ]


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


format : FormatHint -> String
format hint =
    formatHelper 0 hint


formatHelper : Int -> FormatHint -> String
formatHelper indentation hint =
    case hint of
        Str value ->
            applyIndentation indentation value

        Indent strs ->
            strs
                |> List.map (formatHelper (indentation + 4))
                |> String.join "\n"


applyIndentation : Int -> String -> String
applyIndentation indent str =
    String.repeat indent " " ++ str
