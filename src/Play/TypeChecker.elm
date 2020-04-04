module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Play.Data.Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Qualifier as Qualifier


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type alias TypeDefinition =
    { name : String
    , members : List ( String, Type )
    }


type alias WordDefinition =
    { name : String
    , type_ : WordType
    , metadata : Metadata
    , implementation : List AstNode
    }


type AstNode
    = IntLiteral Int
    | Word String WordType
    | ConstructType String
    | SetMember String String Type
    | GetMember String String Type
    | BuiltinPlus
    | BuiltinMinus
    | BuiltinEqual


type alias Context =
    { types : Dict String Qualifier.TypeDefinition
    , typedWords : Dict String WordDefinition
    , untypedWords : Dict String Qualifier.WordDefinition
    , stackEffects : List StackEffect
    , errors : List ()
    }


type StackEffect
    = Push Type
    | Pop Type


initContext : Qualifier.AST -> Context
initContext ast =
    { types = ast.types
    , typedWords = Dict.empty
    , untypedWords = ast.words
    , stackEffects = []
    , errors = []
    }


typeCheck : Qualifier.AST -> Result () AST
typeCheck ast =
    typeCheckHelper (initContext ast) ast


typeCheckHelper : Context -> Qualifier.AST -> Result () AST
typeCheckHelper context ast =
    let
        updatedContext =
            ast
                |> .words
                |> Dict.values
                |> List.foldl typeCheckDefinition context
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = ast.types
            , words = updatedContext.typedWords
            }

    else
        Err ()


typeCheckDefinition : Qualifier.WordDefinition -> Context -> Context
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

                finalContext =
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
            in
            case untypedDef.metadata.type_ of
                Just annotatedType ->
                    if annotatedType /= wordType then
                        { finalContext | errors = () :: finalContext.errors }

                    else
                        finalContext

                Nothing ->
                    finalContext


typeCheckNode : Qualifier.Node -> Context -> Context
typeCheckNode node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = effects ++ ctx.stackEffects }
    in
    case node of
        Qualifier.Integer _ ->
            addStackEffect context [ Push Type.Int ]

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

        Qualifier.ConstructType typeName ->
            case Dict.get typeName context.types of
                Just type_ ->
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = List.map Tuple.second type_.members
                            , output = [ Type.Custom typeName ]
                            }

                Nothing ->
                    Debug.todo "inconcievable!"

        Qualifier.SetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = [ Type.Custom typeName, memberType ]
                            , output = [ Type.Custom typeName ]
                            }

                Nothing ->
                    Debug.todo "inconcievable!"

        Qualifier.GetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = [ Type.Custom typeName ]
                            , output = [ memberType ]
                            }

                Nothing ->
                    Debug.todo "inconcievable!"

        Qualifier.BuiltinPlus ->
            addStackEffect context <| wordTypeToStackEffects { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }

        Qualifier.BuiltinMinus ->
            addStackEffect context <| wordTypeToStackEffects { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }

        Qualifier.BuiltinEqual ->
            addStackEffect context <| wordTypeToStackEffects { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }


wordTypeToStackEffects : WordType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Push wordType.output
        ++ List.map Pop wordType.input


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

        Qualifier.ConstructType typeName ->
            case Dict.get typeName context.types of
                Just _ ->
                    ConstructType typeName

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.SetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    SetMember typeName memberName memberType

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.GetMember typeName memberName ->
            case getMemberType context.types typeName memberName of
                Just memberType ->
                    GetMember typeName memberName memberType

                Nothing ->
                    Debug.todo "Inconcievable!"

        Qualifier.BuiltinPlus ->
            BuiltinPlus

        Qualifier.BuiltinMinus ->
            BuiltinMinus

        Qualifier.BuiltinEqual ->
            BuiltinEqual


getMemberType : Dict String TypeDefinition -> String -> String -> Maybe Type
getMemberType typeDict typeName memberName =
    Dict.get typeName typeDict
        |> Maybe.map .members
        |> Maybe.andThen (List.find (\( name, _ ) -> name == memberName))
        |> Maybe.map Tuple.second
