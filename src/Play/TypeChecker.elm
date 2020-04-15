module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Play.Data.Builtin as Builtin exposing (Builtin)
import Play.Data.Metadata exposing (Metadata)
import Play.Data.Type as Type exposing (Type, WordType)
import Play.Qualifier as Qualifier
import Set exposing (Set)


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
    | Builtin Builtin


type alias Context =
    { types : Dict String TypeDefinition
    , typedWords : Dict String WordDefinition
    , untypedWords : Dict String Qualifier.WordDefinition
    , stackEffects : List StackEffect
    , boundGenerics : Dict String Type
    , errors : List ()
    }


type StackEffect
    = Push Type
    | Pop Type


initContext : Qualifier.AST -> Context
initContext ast =
    let
        concreteTypes =
            ast.types
                |> Dict.values
                |> List.map
                    (\t ->
                        case t of
                            Qualifier.CustomTypeDef name members ->
                                { name = name, members = members }

                            Qualifier.UnionTypeDef name _ ->
                                { name = name, members = [] }
                    )
                |> Dict.fromListBy .name
    in
    { types = concreteTypes
    , typedWords = Dict.empty
    , untypedWords = ast.words
    , stackEffects = []
    , boundGenerics = Dict.empty
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

        concreteTypes =
            ast.types
                |> Dict.values
                |> List.map
                    (\t ->
                        case t of
                            Qualifier.CustomTypeDef name members ->
                                { name = name, members = members }

                            Qualifier.UnionTypeDef name _ ->
                                { name = name, members = [] }
                    )
                |> Dict.fromListBy .name
    in
    if List.isEmpty updatedContext.errors then
        Ok <|
            { types = concreteTypes
            , words = updatedContext.typedWords
            }

    else
        Err ()


typeCheckDefinition : Qualifier.WordDefinition -> Context -> Context
typeCheckDefinition untypedDef context =
    let
        cleanContext =
            { context
                | stackEffects = []
                , boundGenerics = Dict.empty
            }
    in
    case Dict.get untypedDef.name context.typedWords of
        Just _ ->
            cleanContext

        Nothing ->
            let
                impl =
                    case untypedDef.implementation of
                        Qualifier.SoloImpl impl_ ->
                            impl_

                        Qualifier.MultiImpl _ impl_ ->
                            impl_

                contextWithStackEffects =
                    List.foldl typeCheckNode cleanContext impl

                ( contextAfterWordTypeInduction, wordType ) =
                    wordTypeFromStackEffects contextWithStackEffects
                        |> simplifyWordType untypedDef.name

                finalContext =
                    { contextAfterWordTypeInduction
                        | typedWords =
                            Dict.insert untypedDef.name
                                { name = untypedDef.name
                                , type_ = wordType
                                , metadata = untypedDef.metadata
                                , implementation = List.map (untypedToTypedNode contextAfterWordTypeInduction) impl
                                }
                                contextAfterWordTypeInduction.typedWords
                        , boundGenerics = Dict.empty
                        , stackEffects = []
                    }
            in
            case untypedDef.metadata.type_ of
                Just annotatedType ->
                    let
                        ( _, simplifiedAnnotatedType ) =
                            simplifyWordType untypedDef.name ( contextAfterWordTypeInduction, annotatedType )
                    in
                    if simplifiedAnnotatedType /= wordType then
                        { finalContext | errors = () :: finalContext.errors }

                    else
                        finalContext

                Nothing ->
                    finalContext


typeCheckNode : Qualifier.Node -> Context -> Context
typeCheckNode node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = ctx.stackEffects ++ effects }
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

        Qualifier.Builtin builtin ->
            addStackEffect context <| wordTypeToStackEffects <| Builtin.wordType builtin


wordTypeToStackEffects : WordType -> List StackEffect
wordTypeToStackEffects wordType =
    List.map Pop (List.reverse wordType.input)
        ++ List.map Push wordType.output


wordTypeFromStackEffects : Context -> ( Context, WordType )
wordTypeFromStackEffects context =
    wordTypeFromStackEffectsHelper context.stackEffects ( context, { input = [], output = [] } )


wordTypeFromStackEffectsHelper : List StackEffect -> ( Context, WordType ) -> ( Context, WordType )
wordTypeFromStackEffectsHelper effects ( context, wordType ) =
    case effects of
        [] ->
            ( context
              -- TODO: Really need to get this list reverse madness figured out and solved properly
            , { wordType
                | input = wordType.input
                , output = List.reverse wordType.output
              }
            )

        (Pop type_) :: remainingEffects ->
            case wordType.output of
                [] ->
                    wordTypeFromStackEffectsHelper remainingEffects <|
                        ( context, { wordType | input = type_ :: wordType.input } )

                availableType :: remainingOutput ->
                    let
                        ( newContext, compatible ) =
                            compatibleTypes context availableType type_
                    in
                    if not compatible then
                        ( { newContext | errors = () :: context.errors }, wordType )

                    else
                        wordTypeFromStackEffectsHelper remainingEffects <|
                            ( newContext, { wordType | output = remainingOutput } )

        (Push type_) :: remainingEffects ->
            wordTypeFromStackEffectsHelper remainingEffects <|
                ( context, { wordType | output = type_ :: wordType.output } )


compatibleTypes : Context -> Type -> Type -> ( Context, Bool )
compatibleTypes context typeA typeB =
    case ( getGenericBinding context typeA, getGenericBinding context typeB ) of
        -- A is unbound
        ( Nothing, Just boundB ) ->
            ( bindGeneric typeA boundB context, True )

        -- B is unbound
        ( Just boundA, Nothing ) ->
            ( bindGeneric typeB boundA context, True )

        -- Both are unbound
        ( Nothing, Nothing ) ->
            ( bindGeneric typeA typeB context
                |> bindGeneric typeB typeA
            , True
            )

        -- Both types are either a resolved type or bound generic
        ( Just boundA, Just boundB ) ->
            ( context, boundA == boundB )


getGenericBinding : Context -> Type -> Maybe Type
getGenericBinding context type_ =
    case type_ of
        Type.Generic genericId ->
            case Dict.get genericId context.boundGenerics of
                Just (Type.Generic nextGenericId) ->
                    case Dict.get nextGenericId context.boundGenerics of
                        Just ((Type.Generic cycleCheckId) as anotherGenericType) ->
                            if cycleCheckId == genericId then
                                Nothing

                            else
                                getGenericBinding context anotherGenericType

                        otherwise ->
                            otherwise

                otherwise ->
                    otherwise

        _ ->
            Just type_


bindGeneric : Type -> Type -> Context -> Context
bindGeneric toBind target context =
    case toBind of
        Type.Generic name ->
            { context | boundGenerics = Dict.insert name target context.boundGenerics }

        _ ->
            context


simplifyWordType : String -> ( Context, WordType ) -> ( Context, WordType )
simplifyWordType defName ( context, wordType ) =
    let
        oldSignature =
            wordType.input ++ wordType.output

        inputLength =
            List.length wordType.input

        aliases =
            oldSignature
                |> List.filterMap genericName
                -- remove duplicates
                |> (Set.fromList >> Set.toList)
                |> List.map (findAliases context)
                |> List.foldl reverseLookup Dict.empty

        newSignature =
            List.map reduceGenericName oldSignature
                |> List.foldl renameGenerics ( 'a', Dict.empty, [] )
                |> (\( _, _, ns ) -> ns)
                |> List.reverse

        reduceGenericName type_ =
            case type_ of
                Type.Generic genName ->
                    case getGenericBinding context type_ of
                        Just boundType ->
                            boundType

                        Nothing ->
                            case Dict.get genName aliases of
                                Just actualName ->
                                    Type.Generic actualName

                                Nothing ->
                                    type_

                _ ->
                    type_

        renameGenerics type_ ( nextId, seenGenerics, acc ) =
            case type_ of
                Type.Generic genName ->
                    case Dict.get genName seenGenerics of
                        Just newName ->
                            ( nextId, seenGenerics, Type.Generic newName :: acc )

                        Nothing ->
                            let
                                newName =
                                    String.fromChar nextId ++ "_" ++ defName
                            in
                            ( nextId
                                |> Char.toCode
                                |> (+) 1
                                |> Char.fromCode
                            , Dict.insert genName newName seenGenerics
                            , Type.Generic newName :: acc
                            )

                _ ->
                    ( nextId, seenGenerics, type_ :: acc )
    in
    ( context
    , { input = List.take inputLength newSignature
      , output = List.drop inputLength newSignature
      }
    )


genericName : Type -> Maybe String
genericName type_ =
    case type_ of
        Type.Generic name ->
            Just name

        _ ->
            Nothing


findAliases : Context -> String -> ( String, List String )
findAliases context generic =
    ( generic
    , context.boundGenerics
        |> Dict.keys
        |> List.filterMap (\key -> isAliasOf context Set.empty generic key key)
    )


isAliasOf : Context -> Set String -> String -> String -> String -> Maybe String
isAliasOf context visitedKeys targetKey topKey currentKey =
    case Dict.get currentKey context.boundGenerics of
        Just (Type.Generic genericKey) ->
            if Set.member genericKey visitedKeys then
                Nothing

            else if genericKey == targetKey then
                Just topKey

            else
                isAliasOf context (Set.insert currentKey visitedKeys) targetKey topKey genericKey

        _ ->
            Nothing


reverseLookup : ( String, List String ) -> Dict String String -> Dict String String
reverseLookup ( name, aliases ) acc =
    let
        targetName =
            Dict.get name acc
                |> Maybe.withDefault name
    in
    aliases
        |> List.filter (\a -> not <| Dict.member a acc)
        |> List.filter (\a -> a /= targetName)
        |> List.foldl (\alias newAcc -> Dict.insert alias targetName newAcc) acc


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

        Qualifier.Builtin builtin ->
            Builtin builtin


getMemberType : Dict String TypeDefinition -> String -> String -> Maybe Type
getMemberType typeDict typeName memberName =
    Dict.get typeName typeDict
        |> Maybe.map .members
        |> Maybe.andThen (List.find (\( name, _ ) -> name == memberName))
        |> Maybe.map Tuple.second
