module Wasm exposing (..)

import List.Extra as List


type Module
    = Module
        { typeSignatures : List TypeSignature
        , functions : List Function
        , exports : List Int
        , start : Maybe Int
        }


type alias Function =
    { name : String
    , typeSignatureIndex : Int
    , instructions : List Instruction
    }


type alias TypeSignature =
    { inputs : List Type
    , outputs : List Type
    }


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
    | I32_Const Int
    | I32_Add
    | I32_Sub
    | I32_Eq


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
                    "(call " ++ String.fromInt idx ++ ") ;; call $" ++ word

                Nothing ->
                    "(call $" ++ word ++ ")"

        I32_Const num ->
            "(i32.const " ++ String.fromInt num ++ ")"

        I32_Add ->
            "i32.add"

        I32_Sub ->
            "i32.sub"

        I32_Eq ->
            "i32.eq"


initModule : Module
initModule =
    Module
        { typeSignatures = []
        , functions = []
        , exports = []
        , start = Nothing
        }


type alias FunctionDef =
    { name : String
    , args : List Type
    , results : List Type
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
            , instructions = funcDef.instructions
            }
    in
    Module <|
        { updatedModule | functions = updatedModule.functions ++ [ newFunction ] }


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


toString : Module -> String
toString ((Module module_) as fullModule) =
    [ Str "(module"
    , List.concatMap formatTypeSignature module_.typeSignatures
        |> Indent
    , List.concatMap (formatFunction fullModule) module_.functions
        |> Indent
    , List.concatMap formatExport module_.exports
        |> Indent
    , formatStartFunction module_.start
    , Str ")"
    ]
        |> List.map format
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


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
        fullFuncDef =
            String.join " "
                [ "(func"
                , "$" ++ function.name
                , "(type " ++ String.fromInt function.typeSignatureIndex ++ ")"
                ]
    in
    [ Str fullFuncDef
    , Indent <| List.map (Str << instructionToString module_) function.instructions
    , Str ")"
    ]


formatExport : Int -> List FormatHint
formatExport idx =
    Debug.todo "formatExport"


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
