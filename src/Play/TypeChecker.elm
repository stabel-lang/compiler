module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Play.Data.Metadata exposing (Metadata)
import Play.Qualifier as Qualifier


type alias TypedDefinition =
    { name : String
    , type_ : WordType
    , metadata : Metadata
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
                contextWithStackEffects =
                    List.foldl typeCheckNode cleanContext untypedDef.implementation
                        |> (\ctx -> { ctx | stackEffects = List.reverse ctx.stackEffects })

                ( contextAfterWordTypeInduction, wordType ) =
                    wordTypeFromStackEffects contextWithStackEffects
            in
            { contextAfterWordTypeInduction
                | typedWords =
                    Dict.insert untypedDef.name
                        { name = untypedDef.name
                        , type_ = wordType
                        , metadata = untypedDef.metadata
                        , implementation = List.map (untypedToTypedNode contextAfterWordTypeInduction) untypedDef.implementation
                        }
                        contextAfterWordTypeInduction.typedWords
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


wordTypeFromStackEffects : Context -> ( Context, WordType )
wordTypeFromStackEffects context =
    wordTypeFromStackEffectsHelper context.stackEffects ( context, { input = [], output = [] } )


wordTypeFromStackEffectsHelper : List StackEffect -> ( Context, WordType ) -> ( Context, WordType )
wordTypeFromStackEffectsHelper effects ( context, wordType ) =
    case effects of
        [] ->
            ( context, wordType )

        (Pop type_) :: remainingEffects ->
            case wordType.output of
                [] ->
                    wordTypeFromStackEffectsHelper remainingEffects <|
                        ( context, { wordType | input = type_ :: wordType.input } )

                availableType :: remainingOutput ->
                    if availableType /= type_ then
                        ( { context | errors = () :: context.errors }, wordType )

                    else
                        wordTypeFromStackEffectsHelper remainingEffects <|
                            ( context, { wordType | output = remainingOutput } )

        (Push type_) :: remainingEffects ->
            wordTypeFromStackEffectsHelper remainingEffects <|
                ( context, { wordType | output = type_ :: wordType.output } )


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
