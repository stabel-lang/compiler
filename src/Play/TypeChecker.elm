module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Play.Parser as PAST
import Play.Qualifier as Qualifier


type alias TypedDefinition =
    { name : String
    , type_ : WordType
    , metadata : List ( String, List PAST.AstNode )
    , implementation : List AstNode
    }


type Type
    = IntType


type alias WordType =
    { input : List Type
    , output : List Type
    }


type AstNode
    = IntLiteral Int
    | Word String WordType
    | BuiltinPlus
    | BuiltinMinus
    | BuiltinEqual


type alias Context =
    { typedWords : Dict String TypedDefinition
    , untypedWords : Dict String Qualifier.Definition
    , stackEffects : List StackEffect
    , errors : List ()
    }


type StackEffect
    = Push Type
    | Pop Type


initContext : List Qualifier.Definition -> Context
initContext ast =
    { typedWords = Dict.empty
    , untypedWords = List.foldl (\word acc -> Dict.insert word.name word acc) Dict.empty ast
    , stackEffects = []
    , errors = []
    }


typeCheck : List Qualifier.Definition -> Result () (List TypedDefinition)
typeCheck ast =
    typeCheckHelper (initContext ast) ast


typeCheckHelper : Context -> List Qualifier.Definition -> Result () (List TypedDefinition)
typeCheckHelper context ast =
    let
        updatedContext =
            List.foldl typeCheckDefinition context ast
    in
    if List.isEmpty context.errors then
        Ok <| Dict.values updatedContext.typedWords

    else
        Err ()


typeCheckDefinition : Qualifier.Definition -> Context -> Context
typeCheckDefinition untypedDef context =
    let
        cleanContext =
            { context | stackEffects = [] }
    in
    case Dict.get untypedDef.name context.typedWords of
        Just _ ->
            cleanContext

        Nothing ->
            let
                -- Todo resulting stack effects is reversed
                -- Todo this doesn't actually check that the types match
                contextWithStackEffects =
                    List.foldl typeCheckNode cleanContext untypedDef.implementation

                wordType =
                    wordTypeFromStackEffects contextWithStackEffects.stackEffects
            in
            { contextWithStackEffects
                | typedWords =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , type_ = wordType
                        , metadata = untypedDef.metadata
                        , implementation = List.map (untypedToTypedNode contextWithStackEffects) untypedDef.implementation
                        }
                        contextWithStackEffects.typedWords
                , stackEffects = []
            }


typeCheckNode : Qualifier.Node -> Context -> Context
typeCheckNode node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = effects ++ ctx.stackEffects }
    in
    case node of
        Qualifier.Integer _ ->
            addStackEffect context [ Push IntType ]

        Qualifier.Word name ->
            case Dict.get name context.typedWords of
                Just def ->
                    addStackEffect context <| wordTypeToStackEffects def.type_

                Nothing ->
                    case Dict.get name context.untypedWords of
                        Nothing ->
                            Debug.todo "inconcievable!"

                        Just untypedDef ->
                            let
                                contextWithTypedDef =
                                    typeCheckDefinition untypedDef context

                                newContext =
                                    { contextWithTypedDef | stackEffects = context.stackEffects }
                            in
                            case Dict.get name newContext.typedWords of
                                Nothing ->
                                    Debug.todo "inconcievable!"

                                Just def ->
                                    addStackEffect newContext <| wordTypeToStackEffects def.type_

        Qualifier.BuiltinPlus ->
            addStackEffect context <| wordTypeToStackEffects { input = [ IntType, IntType ], output = [ IntType ] }

        Qualifier.BuiltinMinus ->
            addStackEffect context <| wordTypeToStackEffects { input = [ IntType, IntType ], output = [ IntType ] }

        Qualifier.BuiltinEqual ->
            addStackEffect context <| wordTypeToStackEffects { input = [ IntType, IntType ], output = [ IntType ] }


wordTypeToStackEffects : WordType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Pop wordType.input
        ++ List.map Push wordType.output
        |> List.reverse


wordTypeFromStackEffects : List StackEffect -> WordType
wordTypeFromStackEffects effects =
    List.foldr wordTypeFromStackEffectsHelper { input = [], output = [] } effects


wordTypeFromStackEffectsHelper : StackEffect -> WordType -> WordType
wordTypeFromStackEffectsHelper effect wordType =
    case effect of
        Pop IntType ->
            case wordType.output of
                [] ->
                    { wordType | input = IntType :: wordType.input }

                IntType :: remainingOutput ->
                    { wordType | output = remainingOutput }

        Push IntType ->
            { wordType | output = IntType :: wordType.output }


untypedToTypedNode : Context -> Qualifier.Node -> AstNode
untypedToTypedNode context untypedNode =
    case untypedNode of
        Qualifier.Integer num ->
            IntLiteral num

        Qualifier.Word name ->
            case Dict.get name context.typedWords of
                Just def ->
                    Word def.name def.type_

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.BuiltinPlus ->
            BuiltinPlus

        Qualifier.BuiltinMinus ->
            BuiltinMinus

        Qualifier.BuiltinEqual ->
            BuiltinEqual
