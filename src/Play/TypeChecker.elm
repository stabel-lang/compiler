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


type TypeDefinition
    = CustomTypeDef String (List ( String, Type ))
    | UnionTypeDef String (List Type)


type alias WordDefinition =
    { name : String
    , type_ : WordType
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List AstNode)
    | MultiImpl (List ( Type, List AstNode )) (List AstNode)


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
                                CustomTypeDef name members

                            Qualifier.UnionTypeDef name memberTypes ->
                                UnionTypeDef name memberTypes
                    )
                |> Dict.fromListBy typeDefName
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
            ast.words
                |> Dict.values
                |> List.foldl typeCheckDefinition context

        concreteTypes =
            ast.types
                |> Dict.values
                |> List.map
                    (\t ->
                        case t of
                            Qualifier.CustomTypeDef name members ->
                                CustomTypeDef name members

                            Qualifier.UnionTypeDef name memberTypes ->
                                UnionTypeDef name memberTypes
                    )
                |> Dict.fromListBy typeDefName
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
        cleanContext ctx =
            { ctx
                | stackEffects = []
                , boundGenerics = Dict.empty
            }
    in
    case Dict.get untypedDef.name context.typedWords of
        Just _ ->
            cleanContext context

        Nothing ->
            case untypedDef.implementation of
                Qualifier.SoloImpl impl ->
                    let
                        ( inferredType, newContext ) =
                            typeCheckImplementation untypedDef impl (cleanContext context)

                        finalContext =
                            { newContext
                                | typedWords =
                                    Dict.insert untypedDef.name
                                        { name = untypedDef.name
                                        , type_ =
                                            case untypedDef.metadata.type_ of
                                                Just annotatedType ->
                                                    annotatedType

                                                Nothing ->
                                                    inferredType
                                        , metadata = untypedDef.metadata
                                        , implementation = SoloImpl (List.map (untypedToTypedNode newContext) impl)
                                        }
                                        newContext.typedWords
                            }
                    in
                    verifyTypeSignature inferredType untypedDef finalContext
                        |> cleanContext

                Qualifier.MultiImpl initialWhens defaultImpl ->
                    let
                        whens =
                            case defaultImpl of
                                [] ->
                                    initialWhens

                                _ ->
                                    let
                                        ( inferredDefaultType, _ ) =
                                            typeCheckImplementation untypedDef defaultImpl (cleanContext context)
                                    in
                                    case inferredDefaultType.input of
                                        [] ->
                                            Debug.todo "Default impl doesn't have an input argument"

                                        firstType :: _ ->
                                            ( firstType, defaultImpl ) :: initialWhens

                        inferWhenTypes ( _, im ) ( infs, ctx ) =
                            let
                                ( inf, newCtx ) =
                                    typeCheckImplementation untypedDef im (cleanContext ctx)
                            in
                            ( inf :: infs, newCtx )

                        ( inferredWhenTypes, newContext ) =
                            List.foldr inferWhenTypes ( [], context ) whens

                        whensAreConsistent =
                            inferredWhenTypes
                                |> List.map2 Tuple.pair (List.map Tuple.first whens)
                                |> List.map typeCheckWhen
                                |> List.all (\( b, _, _ ) -> b)

                        whensAreCompatible =
                            inferredWhenTypes
                                |> List.map stripFirstInput
                                |> List.map countOutput
                                |> areAllEqual

                        typeCheckWhen ( forType, inf ) =
                            case inf.input of
                                firstInput :: _ ->
                                    let
                                        compatible =
                                            case firstInput of
                                                Type.Generic _ ->
                                                    True

                                                _ ->
                                                    firstInput == forType
                                    in
                                    ( compatible, forType, inf )

                                [] ->
                                    ( False, forType, inf )

                        stripFirstInput inf =
                            case inf.input of
                                _ :: rem ->
                                    { inf | input = rem }

                                _ ->
                                    inf

                        countOutput wordType =
                            ( wordType.input, List.length wordType.output )

                        areAllEqual ls =
                            case ls of
                                [] ->
                                    True

                                first :: rest ->
                                    List.all ((==) first) rest

                        inferredType =
                            List.head inferredWhenTypes
                                |> Maybe.withDefault { input = [], output = [] }
                                |> replaceFirstType (Type.Union (List.map Tuple.first whens))
                                |> joinOutputs (List.map .output inferredWhenTypes)

                        replaceFirstType with inf =
                            case inf.input of
                                _ :: rem ->
                                    { inf | input = with :: rem }

                                _ ->
                                    inf

                        joinOutputs outputs result =
                            case outputs of
                                first :: second :: rest ->
                                    let
                                        joined =
                                            List.map2 unionize first second

                                        unionize lhs rhs =
                                            case ( lhs, rhs ) of
                                                _ ->
                                                    if lhs == rhs then
                                                        lhs

                                                    else
                                                        Type.Union [ lhs, rhs ]
                                    in
                                    joinOutputs (joined :: rest) result

                                joined :: [] ->
                                    { result | output = joined }

                                _ ->
                                    result

                        finalContext =
                            { newContext
                                | typedWords =
                                    Dict.insert untypedDef.name
                                        { name = untypedDef.name
                                        , type_ =
                                            case untypedDef.metadata.type_ of
                                                Just annotatedType ->
                                                    { annotatedType
                                                        | input = List.map (resolveUnion newContext) annotatedType.input
                                                        , output = List.map (resolveUnion newContext) annotatedType.output
                                                    }

                                                Nothing ->
                                                    inferredType
                                        , metadata = untypedDef.metadata
                                        , implementation =
                                            MultiImpl
                                                (List.map (Tuple.mapSecond (List.map (untypedToTypedNode newContext))) whens)
                                                (List.map (untypedToTypedNode newContext) defaultImpl)
                                        }
                                        newContext.typedWords
                                , errors =
                                    if whensAreConsistent && whensAreCompatible then
                                        newContext.errors

                                    else
                                        () :: newContext.errors
                            }
                    in
                    verifyTypeSignature inferredType untypedDef finalContext
                        |> cleanContext


resolveUnion : Context -> Type -> Type
resolveUnion context type_ =
    case type_ of
        Type.Custom typeName ->
            case Dict.get typeName context.types of
                Just (UnionTypeDef _ members) ->
                    Type.Union members

                _ ->
                    type_

        _ ->
            type_


typeCheckImplementation : Qualifier.WordDefinition -> List Qualifier.Node -> Context -> ( WordType, Context )
typeCheckImplementation untypedDef impl context =
    let
        ( _, contextWithStackEffects ) =
            List.foldl
                (\node ( idx, ctx ) -> ( idx + 1, typeCheckNode idx node ctx ))
                ( 0, context )
                impl
    in
    wordTypeFromStackEffects contextWithStackEffects
        |> simplifyWordType untypedDef.name
        |> (\( a, b ) -> ( b, a ))


verifyTypeSignature : WordType -> Qualifier.WordDefinition -> Context -> Context
verifyTypeSignature inferredType untypedDef context =
    case untypedDef.metadata.type_ of
        Just annotatedType ->
            let
                ( _, simplifiedAnnotatedType ) =
                    simplifyWordType untypedDef.name ( context, annotatedType )
            in
            if not <| compatibleWordTypes simplifiedAnnotatedType inferredType context then
                { context | errors = () :: context.errors }

            else
                context

        Nothing ->
            context


compatibleWordTypes : WordType -> WordType -> Context -> Bool
compatibleWordTypes annotated inferred context =
    let
        annotatedTypes =
            annotated.input
                ++ annotated.output
                |> List.map (resolveUnion context)

        inferredTypes =
            inferred.input
                ++ inferred.output
                |> List.map (resolveUnion context)

        compareType lhs rhs =
            case ( lhs, rhs ) of
                ( Type.Generic _, _ ) ->
                    True

                ( _, Type.Generic _ ) ->
                    True

                ( Type.Union lMembers, Type.Union rMembers ) ->
                    let
                        lSet =
                            Set.fromList (List.map typeAsStr lMembers)

                        rSet =
                            Set.fromList (List.map typeAsStr rMembers)

                        diff =
                            Set.diff rSet lSet
                                |> Set.toList
                    in
                    case diff of
                        [] ->
                            True

                        [ oneDiff ] ->
                            -- Likely the default case
                            String.endsWith "_Generic" oneDiff

                        _ ->
                            False

                ( Type.Union _, _ ) ->
                    -- Cannot go from union to concrete type
                    False

                ( _, Type.Union rMembers ) ->
                    rMembers
                        |> List.map typeAsStr
                        |> Set.fromList
                        |> Set.member (typeAsStr lhs)

                _ ->
                    lhs == rhs
    in
    if
        (List.length annotated.input /= List.length inferred.input)
            || (List.length annotated.output /= List.length inferred.output)
    then
        False

    else
        List.map2 compareType annotatedTypes inferredTypes
            |> List.all identity


typeAsStr : Type -> String
typeAsStr t =
    case t of
        Type.Custom name ->
            name ++ "_Custom"

        Type.Int ->
            "Int"

        Type.Union _ ->
            "Union"

        Type.Generic name ->
            name ++ "_Generic"


typeCheckNode : Int -> Qualifier.Node -> Context -> Context
typeCheckNode idx node context =
    let
        addStackEffect ctx effects =
            { ctx | stackEffects = ctx.stackEffects ++ List.map tagGeneric effects }

        tagGeneric effect =
            case effect of
                Push (Type.Generic genName) ->
                    Push (Type.Generic (genName ++ String.fromInt idx))

                Pop (Type.Generic genName) ->
                    Pop (Type.Generic (genName ++ String.fromInt idx))

                _ ->
                    effect
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
                Just (CustomTypeDef _ members) ->
                    addStackEffect context <|
                        wordTypeToStackEffects
                            { input = List.map Tuple.second members
                            , output = [ Type.Custom typeName ]
                            }

                _ ->
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
            if boundA == boundB then
                ( context, True )

            else
                case ( boundA, boundB ) of
                    ( Type.Union _, _ ) ->
                        -- Cannot go from union to concrete type
                        -- And we currently require unions to be exact matches
                        ( context, False )

                    ( _, Type.Union unionTypes ) ->
                        if List.member boundA unionTypes then
                            ( context, True )

                        else
                            let
                                isGeneric t =
                                    case t of
                                        Type.Generic _ ->
                                            True

                                        _ ->
                                            False
                            in
                            case List.find isGeneric unionTypes of
                                Just generic ->
                                    compatibleTypes context typeA generic

                                Nothing ->
                                    ( context, False )

                    _ ->
                        ( context, False )


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
    case Dict.get typeName typeDict of
        Just (CustomTypeDef _ members) ->
            List.find (\( name, _ ) -> name == memberName) members
                |> Maybe.map Tuple.second

        _ ->
            Nothing


typeDefName : TypeDefinition -> String
typeDefName typeDef =
    case typeDef of
        CustomTypeDef name _ ->
            name

        UnionTypeDef name _ ->
            name
