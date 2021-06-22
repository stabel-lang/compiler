module Stabel.Qualifier exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Result.Extra as Result
import Set exposing (Set)
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.Metadata as Metadata exposing (Metadata)
import Stabel.Data.Type as Type exposing (Type, WordType)
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.Parser as Parser
import Stabel.Qualifier.Problem exposing (Problem(..))
import Stabel.Qualifier.SourceLocation as SourceLocation exposing (SourceLocationRange)


type alias AST =
    { types : Dict String TypeDefinition
    , words : Dict String WordDefinition
    }


type
    TypeDefinition
    -- TODO: Each branch here should take a record. Too many arguments.
    = CustomTypeDef String Bool SourceLocationRange (List String) (List ( String, Type ))
    | UnionTypeDef String Bool SourceLocationRange (List String) (List Type)


type alias WordDefinition =
    { name : String
    , metadata : Metadata
    , implementation : WordImplementation
    }


type WordImplementation
    = SoloImpl (List Node)
    | MultiImpl (List ( TypeMatch, List Node )) (List Node)


type TypeMatch
    = TypeMatch SourceLocationRange Type (List ( String, TypeMatchValue ))


type TypeMatchValue
    = LiteralInt Int
    | LiteralType Type
    | RecursiveMatch TypeMatch


type Node
    = Integer SourceLocationRange Int
    | Word SourceLocationRange String
    | WordRef SourceLocationRange String
    | ConstructType String
    | GetMember String String
    | SetMember String String
    | Builtin SourceLocationRange Builtin


type alias ModuleReferences =
    { aliases : Dict String String
    , imports : Dict String (List String)
    }


builtinDict : Dict String Builtin
builtinDict =
    Dict.fromList
        [ ( "+", Builtin.Plus )
        , ( "-", Builtin.Minus )
        , ( "*", Builtin.Multiply )
        , ( "div", Builtin.Divide )
        , ( "=", Builtin.Equal )
        , ( "swap", Builtin.StackSwap )
        , ( "dup", Builtin.StackDuplicate )
        , ( "drop", Builtin.StackDrop )
        , ( "rotate", Builtin.StackRightRotate )
        , ( "-rotate", Builtin.StackLeftRotate )
        , ( "!", Builtin.Apply )
        ]


type alias RunConfig =
    { packageName : String
    , modulePath : String
    , ast : Parser.AST
    , externalModules : Dict String String
    , inProgressAST : AST
    }


run : RunConfig -> Result (List Problem) AST
run config =
    let
        ( typeErrors, qualifiedTypes ) =
            Dict.foldl (\_ val acc -> qualifyType config val acc) ( [], Dict.empty ) config.ast.types
                |> Tuple.mapSecond (\qt -> Dict.map (\_ v -> resolveUnionInTypeDefs qt v) qt)

        allQualifiedTypes =
            Dict.union qualifiedTypes config.inProgressAST.types

        ( wordErrors, qualifiedWords ) =
            Dict.foldl (\_ val acc -> qualifyDefinition config allQualifiedTypes val acc) ( [], Dict.empty ) config.ast.functions
    in
    case ( typeErrors, wordErrors ) of
        ( [], [] ) ->
            Ok
                { types = qualifiedTypes
                , words = qualifiedWords
                }

        _ ->
            Err <| typeErrors ++ wordErrors


type alias ModuleDefinitionConfig a =
    { a
        | ast : Parser.AST
        , externalModules : Dict String String
    }


moduleDefinition : ModuleDefinitionConfig a -> Parser.ModuleDefinitionRec
moduleDefinition config =
    let
        defaultImports =
            if Dict.get "/core" config.externalModules == Just "stabel/standard_library" then
                Dict.fromList [ ( "/core", [] ) ]

            else
                Dict.empty
    in
    case config.ast.moduleDefinition of
        Parser.Undefined ->
            { imports = defaultImports
            , aliases = Dict.empty
            , exposes = Set.empty
            }

        Parser.Defined def ->
            { imports = Dict.union def.imports defaultImports
            , aliases = def.aliases
            , exposes = def.exposes
            }


resolveUnionInTypeDefs : Dict String TypeDefinition -> TypeDefinition -> TypeDefinition
resolveUnionInTypeDefs qt td =
    case td of
        CustomTypeDef exposed name range generics members ->
            CustomTypeDef exposed name range generics (List.map (Tuple.mapSecond (resolveUnion qt)) members)

        UnionTypeDef exposed name range generics memberTypes ->
            UnionTypeDef exposed name range generics (List.map (resolveUnion qt) memberTypes)


resolveUnion : Dict String TypeDefinition -> Type -> Type
resolveUnion typeDefs type_ =
    case type_ of
        Type.Custom typeName ->
            case Dict.get typeName typeDefs of
                Just (UnionTypeDef _ _ _ _ members) ->
                    Type.Union members

                _ ->
                    type_

        Type.CustomGeneric typeName types ->
            case Dict.get typeName typeDefs of
                Just (UnionTypeDef _ _ _ generics members) ->
                    let
                        genericsMap =
                            List.map2 Tuple.pair generics types
                                |> Dict.fromList

                        rebindGenerics t =
                            case t of
                                Type.Generic val ->
                                    Dict.get val genericsMap
                                        |> Maybe.withDefault t

                                Type.CustomGeneric cgName cgMembers ->
                                    Type.CustomGeneric cgName <|
                                        List.map rebindGenerics cgMembers

                                _ ->
                                    t
                    in
                    Type.Union (List.map rebindGenerics members)

                _ ->
                    type_

        _ ->
            type_


qualifyType :
    RunConfig
    -> Parser.TypeDefinition
    -> ( List Problem, Dict String TypeDefinition )
    -> ( List Problem, Dict String TypeDefinition )
qualifyType config typeDef ( errors, acc ) =
    let
        modDef =
            moduleDefinition config

        ( modRefs, exposes ) =
            ( { aliases = modDef.aliases
              , imports = modDef.imports
              }
            , modDef.exposes
            )
    in
    case typeDef of
        Parser.CustomTypeDef range name generics members ->
            let
                qualifiedName =
                    qualifyName config name

                qualifiedMemberResult =
                    List.map (Tuple.mapSecond (qualifyMemberType config modRefs range)) members
                        |> List.map raiseTupleError
                        |> Result.combine

                exposed =
                    Set.isEmpty exposes || Set.member name exposes

                raiseTupleError ( label, result ) =
                    case result of
                        Ok value ->
                            Ok ( label, value )

                        Err err ->
                            Err err
            in
            case qualifiedMemberResult of
                Err err ->
                    ( err :: errors
                    , acc
                    )

                Ok qualifiedMembers ->
                    ( errors
                    , Dict.insert qualifiedName
                        (CustomTypeDef
                            qualifiedName
                            exposed
                            range
                            generics
                            qualifiedMembers
                        )
                        acc
                    )

        Parser.UnionTypeDef range name generics memberTypes ->
            let
                qualifiedName =
                    qualifyName config name

                exposed =
                    Set.isEmpty exposes || Set.member name exposes

                qualifiedMemberTypesResult =
                    List.map (qualifyMemberType config modRefs range) memberTypes
                        |> Result.combine
            in
            case qualifiedMemberTypesResult of
                Err err ->
                    ( err :: errors
                    , acc
                    )

                Ok qualifiedMemberTypes ->
                    ( errors
                    , Dict.insert qualifiedName
                        (UnionTypeDef
                            qualifiedName
                            exposed
                            range
                            generics
                            qualifiedMemberTypes
                        )
                        acc
                    )


qualifyMemberType :
    RunConfig
    -> ModuleReferences
    -> SourceLocationRange
    -> Parser.PossiblyQualifiedType
    -> Result Problem Type
qualifyMemberType config modRefs range type_ =
    let
        internalRefLookup path name binds =
            let
                qualifiedName =
                    path
                        ++ [ name ]
                        |> String.join "/"
                        |> qualifyPackageModule config.packageName

                bindResult =
                    binds
                        |> List.map (qualifyMemberType config modRefs range)
                        |> Result.combine
            in
            case ( Dict.get qualifiedName config.inProgressAST.types, bindResult ) of
                ( Just (CustomTypeDef _ False _ _ _), _ ) ->
                    Err <| TypeNotExposed range qualifiedName

                ( Just (CustomTypeDef _ True _ [] _), _ ) ->
                    Ok <| Type.Custom qualifiedName

                ( Just (CustomTypeDef _ True _ _ _), Ok qualifiedBinds ) ->
                    Ok <| Type.CustomGeneric qualifiedName qualifiedBinds

                ( Just (UnionTypeDef _ True _ _ memberTypes), _ ) ->
                    Ok <| Type.Union memberTypes

                ( Just (UnionTypeDef _ False _ _ _), _ ) ->
                    Err <| TypeNotExposed range qualifiedName

                _ ->
                    Err <| UnknownTypeRef range qualifiedName

        importsLookup name binds =
            case resolveImportedType config modRefs name of
                Just importedModule ->
                    if String.startsWith "/" importedModule then
                        let
                            nextPath =
                                importedModule
                                    |> String.dropLeft 1
                                    |> String.split "/"
                                    -- Drop author/packageName part
                                    |> List.drop 2
                        in
                        qualifyMemberType config modRefs range <|
                            Parser.ExternalRef nextPath name binds

                    else
                        qualifyMemberType config modRefs range <|
                            Parser.InternalRef (String.split "/" importedModule) name binds

                Nothing ->
                    Err <| UnknownTypeRef range name
    in
    case type_ of
        Parser.LocalRef "Int" [] ->
            Ok <| Type.Int

        Parser.LocalRef name [] ->
            case Dict.get name config.ast.types of
                Just _ ->
                    Ok <| Type.Custom (qualifyName config name)

                Nothing ->
                    importsLookup name []

        Parser.LocalRef name binds ->
            case Dict.get name config.ast.types of
                Just _ ->
                    let
                        bindResult =
                            binds
                                |> List.map (qualifyMemberType config modRefs range)
                                |> Result.combine
                    in
                    case bindResult of
                        Ok convertedBindings ->
                            Ok <|
                                Type.CustomGeneric
                                    (qualifyName config name)
                                    convertedBindings

                        Err err ->
                            Err err

                Nothing ->
                    importsLookup name binds

        Parser.InternalRef ([ possibleAlias ] as path) name binds ->
            case Dict.get possibleAlias modRefs.aliases of
                Just val ->
                    if String.startsWith "/" val then
                        let
                            newPath =
                                val
                                    |> String.split "/"
                                    |> List.drop 1
                        in
                        qualifyMemberType config modRefs range <|
                            Parser.ExternalRef newPath name binds

                    else
                        internalRefLookup (String.split "/" val) name binds

                Nothing ->
                    internalRefLookup path name binds

        Parser.InternalRef path name binds ->
            internalRefLookup path name binds

        Parser.ExternalRef path name binds ->
            let
                pathString =
                    "/" ++ String.join "/" path

                qualifiedName =
                    Dict.get pathString config.externalModules
                        |> Maybe.map (\prefix -> "/" ++ prefix ++ pathString ++ "/" ++ name)
                        |> Maybe.withDefault ""

                bindResult =
                    binds
                        |> List.map (qualifyMemberType config modRefs range)
                        |> Result.combine
            in
            case ( Dict.get qualifiedName config.inProgressAST.types, bindResult ) of
                ( Just (CustomTypeDef _ False _ _ _), _ ) ->
                    Err <| TypeNotExposed range qualifiedName

                ( Just (CustomTypeDef _ True _ [] _), _ ) ->
                    Ok <| Type.Custom qualifiedName

                ( Just (CustomTypeDef _ True _ _ _), Ok qualifiedBinds ) ->
                    Ok <| Type.CustomGeneric qualifiedName qualifiedBinds

                ( Just (UnionTypeDef _ True _ _ memberTypes), _ ) ->
                    Ok <| Type.Union memberTypes

                ( Just (UnionTypeDef _ False _ _ _), _ ) ->
                    Err <| TypeNotExposed range qualifiedName

                _ ->
                    Err <| UnknownTypeRef range qualifiedName

        Parser.Generic sym ->
            Ok (Type.Generic sym)

        Parser.StackRange sym ->
            Ok (Type.Generic sym)

        Parser.FunctionType sign ->
            let
                inputResult =
                    sign.input
                        |> List.map (qualifyMemberType config modRefs range)
                        |> Result.combine

                outputResult =
                    sign.output
                        |> List.map (qualifyMemberType config modRefs range)
                        |> Result.combine
            in
            case ( inputResult, outputResult ) of
                ( Ok input, Ok output ) ->
                    Ok <|
                        Type.Quotation
                            { input = input
                            , output = output
                            }

                ( Err input, _ ) ->
                    Err input

                ( _, Err output ) ->
                    Err output


qualifyDefinition :
    RunConfig
    -> Dict String TypeDefinition
    -> Parser.FunctionDefinition
    -> ( List Problem, Dict String WordDefinition )
    -> ( List Problem, Dict String WordDefinition )
qualifyDefinition config qualifiedTypes unqualifiedWord ( errors, acc ) =
    let
        ( whens, impl ) =
            case unqualifiedWord.implementation of
                Parser.SoloImpl defImpl ->
                    ( [], defImpl )

                Parser.MultiImpl whenImpl defImpl ->
                    ( whenImpl, defImpl )

        modDef =
            moduleDefinition config

        moduleReferences =
            { aliases = Dict.union unqualifiedWord.aliases modDef.aliases
            , imports = Dict.union unqualifiedWord.imports modDef.imports
            }

        ( newWordsAfterWhens, qualifiedWhensResult ) =
            whens
                |> List.foldr
                    (qualifyWhen
                        config
                        qualifiedTypes
                        unqualifiedWord.name
                        moduleReferences
                    )
                    ( acc, [] )
                |> Tuple.mapSecond Result.combine

        ( newWordsAfterImpl, qualifiedImplementationResult ) =
            initQualifyNode config unqualifiedWord.name moduleReferences newWordsAfterWhens impl

        qualifiedMetadataResult =
            qualifyMetadata config qualifiedTypes unqualifiedWord

        qualifiedName =
            qualifyName config unqualifiedWord.name
    in
    case ( qualifiedWhensResult, qualifiedImplementationResult, qualifiedMetadataResult ) of
        ( Ok qualifiedWhens, Ok qualifiedImplementation, Ok qualifiedMetadata ) ->
            ( errors
            , Dict.insert qualifiedName
                { name = qualifiedName
                , metadata = qualifiedMetadata
                , implementation =
                    if List.isEmpty qualifiedWhens then
                        SoloImpl qualifiedImplementation

                    else
                        MultiImpl qualifiedWhens qualifiedImplementation
                }
                newWordsAfterImpl
            )

        ( Err whenError, _, _ ) ->
            ( whenError :: errors
            , newWordsAfterImpl
            )

        ( _, Err implError, _ ) ->
            ( implError :: errors
            , newWordsAfterImpl
            )

        ( _, _, Err metaError ) ->
            ( metaError :: errors
            , newWordsAfterImpl
            )


qualifyMetadata :
    RunConfig
    -> Dict String TypeDefinition
    -> Parser.FunctionDefinition
    -> Result Problem Metadata
qualifyMetadata config qualifiedTypes word =
    let
        wordRange =
            Maybe.withDefault SourceLocation.emptyRange word.sourceLocationRange

        inputLength =
            case word.typeSignature of
                Parser.NotProvided ->
                    0

                Parser.UserProvided wt ->
                    List.length wt.input

                Parser.Verified wt ->
                    List.length wt.input

        modDef =
            moduleDefinition config

        modRefs =
            { aliases = Dict.union modDef.aliases word.aliases
            , imports = Dict.union modDef.imports word.imports
            }
    in
    Parser.typeSignatureToMaybe word.typeSignature
        |> Maybe.map (\ts -> ts.input ++ ts.output)
        |> Maybe.withDefault []
        |> List.map (qualifyMemberType config modRefs wordRange)
        |> Result.combine
        |> Result.map
            (\qualifiedFlatTypeSignature ->
                let
                    wordType =
                        { input = List.take inputLength qualifiedFlatTypeSignature
                        , output = List.drop inputLength qualifiedFlatTypeSignature
                        }

                    ts =
                        case word.typeSignature of
                            Parser.NotProvided ->
                                TypeSignature.NotProvided

                            Parser.UserProvided _ ->
                                TypeSignature.UserProvided wordType

                            Parser.Verified _ ->
                                TypeSignature.CompilerProvided wordType

                    baseMeta =
                        Metadata.default
                in
                { baseMeta
                    | type_ =
                        TypeSignature.map (resolveUnions qualifiedTypes) ts
                    , isExposed =
                        case config.ast.moduleDefinition of
                            Parser.Undefined ->
                                True

                            Parser.Defined def ->
                                Set.member word.name def.exposes
                }
            )


validateTypeReferences : RunConfig -> Dict String TypeDefinition -> SourceLocationRange -> Type -> Result Problem Type
validateTypeReferences config typeDefs wordRange type_ =
    case type_ of
        Type.Custom typeName ->
            let
                qualifiedName =
                    qualifyName config typeName
            in
            case Dict.get qualifiedName typeDefs of
                Just _ ->
                    Ok <| Type.Custom qualifiedName

                Nothing ->
                    Err <| UnknownTypeRef wordRange qualifiedName

        Type.CustomGeneric typeName types ->
            let
                qualifiedName =
                    qualifyName config typeName
            in
            case Dict.get qualifiedName typeDefs of
                Just _ ->
                    Ok <| Type.CustomGeneric qualifiedName types

                Nothing ->
                    Err <| UnknownTypeRef wordRange qualifiedName

        _ ->
            Ok type_


resolveUnions : Dict String TypeDefinition -> WordType -> WordType
resolveUnions typeDefs wt =
    { input = List.map (resolveUnion typeDefs) wt.input
    , output = List.map (resolveUnion typeDefs) wt.output
    }


qualifyWhen :
    RunConfig
    -> Dict String TypeDefinition
    -> String
    -> ModuleReferences
    -> ( Parser.TypeMatch, List Parser.AstNode )
    -> ( Dict String WordDefinition, List (Result Problem ( TypeMatch, List Node )) )
    -> ( Dict String WordDefinition, List (Result Problem ( TypeMatch, List Node )) )
qualifyWhen config qualifiedTypes wordName modRefs ( typeMatch, impl ) ( qualifiedWords, result ) =
    let
        ( newWords, qualifiedImplementationResult ) =
            initQualifyNode config wordName modRefs qualifiedWords impl

        qualifiedMatchResult =
            qualifyMatch config qualifiedTypes modRefs typeMatch
    in
    case ( qualifiedImplementationResult, qualifiedMatchResult ) of
        ( Err err, _ ) ->
            ( newWords
            , Err err :: result
            )

        ( _, Err err ) ->
            ( newWords
            , Err err :: result
            )

        ( Ok qualifiedImplementation, Ok qualifiedMatch ) ->
            ( newWords
            , Ok ( qualifiedMatch, qualifiedImplementation ) :: result
            )


qualifyMatch :
    RunConfig
    -> Dict String TypeDefinition
    -> ModuleReferences
    -> Parser.TypeMatch
    -> Result Problem TypeMatch
qualifyMatch config qualifiedTypes modRefs typeMatch =
    let
        qualifiedNameToMatch range name patterns =
            case Dict.get name qualifiedTypes of
                Just (CustomTypeDef _ False _ _ _) ->
                    Err <| TypeNotExposed range name

                Just (CustomTypeDef _ True _ gens members) ->
                    let
                        memberNames =
                            members
                                |> List.map Tuple.first
                                |> Set.fromList

                        qualifiedPatternsResult =
                            patterns
                                |> List.map
                                    (qualifyMatchValue
                                        config
                                        qualifiedTypes
                                        modRefs
                                        range
                                        name
                                        memberNames
                                    )
                                |> Result.combine

                        actualType =
                            case gens of
                                [] ->
                                    Type.Custom name

                                _ ->
                                    Type.CustomGeneric name (List.map Type.Generic gens)
                    in
                    case qualifiedPatternsResult of
                        Ok qualifiedPatterns ->
                            Ok <| TypeMatch range actualType qualifiedPatterns

                        Err err ->
                            Err err

                Just (UnionTypeDef _ False _ _ _) ->
                    Err <| TypeNotExposed range name

                Just (UnionTypeDef _ True _ _ types) ->
                    if List.isEmpty patterns then
                        Ok <| TypeMatch range (Type.Union types) []

                    else
                        Err <| UnionTypeMatchWithPatterns range

                Nothing ->
                    Err <| UnknownTypeRef range name
    in
    case typeMatch of
        Parser.TypeMatch range (Parser.LocalRef "Int" []) [] ->
            Ok <| TypeMatch range Type.Int []

        Parser.TypeMatch range (Parser.LocalRef "Int" []) [ ( "value", Parser.LiteralInt val ) ] ->
            Ok <| TypeMatch range Type.Int [ ( "value", LiteralInt val ) ]

        Parser.TypeMatch range (Parser.Generic sym) [] ->
            Ok <| TypeMatch range (Type.Generic sym) []

        Parser.TypeMatch range (Parser.LocalRef name []) patterns ->
            case qualifiedNameToMatch range (qualifyName config name) patterns of
                (Err (UnknownTypeRef _ _)) as errMsg ->
                    case resolveImportedType config modRefs name of
                        Just importedModule ->
                            qualifiedNameToMatch range (importedModule ++ "/" ++ name) patterns

                        Nothing ->
                            errMsg

                result ->
                    result

        Parser.TypeMatch range (Parser.InternalRef [ possibleAlias ] name _) patterns ->
            case Dict.get possibleAlias modRefs.aliases of
                Just actualPath ->
                    if String.startsWith "/" actualPath then
                        let
                            extPath =
                                actualPath
                                    |> String.dropLeft 1
                                    |> String.split "/"
                        in
                        qualifyMatch config qualifiedTypes modRefs <|
                            Parser.TypeMatch range (Parser.ExternalRef extPath name []) patterns

                    else
                        let
                            qualifiedName =
                                actualPath
                                    ++ "/"
                                    ++ name
                                    |> qualifyPackageModule config.packageName
                        in
                        qualifiedNameToMatch range qualifiedName patterns

                Nothing ->
                    let
                        qualifiedName =
                            possibleAlias
                                ++ "/"
                                ++ name
                                |> qualifyPackageModule config.packageName
                    in
                    qualifiedNameToMatch range qualifiedName patterns

        Parser.TypeMatch range (Parser.InternalRef path name _) patterns ->
            let
                qualifiedName =
                    path
                        ++ [ name ]
                        |> String.join "/"
                        |> qualifyPackageModule config.packageName
            in
            qualifiedNameToMatch range qualifiedName patterns

        Parser.TypeMatch range (Parser.ExternalRef path name _) patterns ->
            let
                pathString =
                    "/" ++ String.join "/" path

                qualifiedName =
                    Dict.get pathString config.externalModules
                        |> Maybe.map (\prefix -> "/" ++ prefix ++ pathString ++ "/" ++ name)
                        |> Maybe.withDefault ""
            in
            qualifiedNameToMatch range qualifiedName patterns

        Parser.TypeMatch range _ _ ->
            Err <| InvalidTypeMatch range


qualifyMatchValue :
    RunConfig
    -> Dict String TypeDefinition
    -> ModuleReferences
    -> SourceLocationRange
    -> String
    -> Set String
    -> ( String, Parser.TypeMatchValue )
    -> Result Problem ( String, TypeMatchValue )
qualifyMatchValue config qualifiedTypes modRefs range typeName memberNames ( fieldName, matchValue ) =
    if Set.member fieldName memberNames then
        case matchValue of
            Parser.LiteralInt val ->
                Ok <| ( fieldName, LiteralInt val )

            Parser.LiteralType type_ ->
                let
                    qualifyTypeResult =
                        qualifyMemberType config modRefs range type_
                in
                case qualifyTypeResult of
                    Ok qualifiedType ->
                        Ok <| ( fieldName, LiteralType qualifiedType )

                    Err err ->
                        Err err

            Parser.RecursiveMatch typeMatch ->
                case qualifyMatch config qualifiedTypes modRefs typeMatch of
                    Err err ->
                        Err err

                    Ok match ->
                        Ok <| ( fieldName, RecursiveMatch match )

    else
        Err <| NoSuchMemberOnType range typeName fieldName


initQualifyNode :
    RunConfig
    -> String
    -> ModuleReferences
    -> Dict String WordDefinition
    -> List Parser.AstNode
    -> ( Dict String WordDefinition, Result Problem (List Node) )
initQualifyNode config currentDefName modRefs qualifiedWords impl =
    List.foldr
        (qualifyNode config currentDefName modRefs)
        (initQualifyNodeAccumulator qualifiedWords)
        impl
        |> (\acc -> ( acc.qualifiedWords, Result.combine acc.qualifiedNodes ))


type alias QualifyNodeAccumulator =
    { availableQuoteId : Int
    , qualifiedWords : Dict String WordDefinition
    , qualifiedNodes : List (Result Problem Node)
    }


initQualifyNodeAccumulator : Dict String WordDefinition -> QualifyNodeAccumulator
initQualifyNodeAccumulator qualifiedWords =
    { availableQuoteId = 1
    , qualifiedWords = qualifiedWords
    , qualifiedNodes = []
    }


qualifyNode :
    RunConfig
    -> String
    -> ModuleReferences
    -> Parser.AstNode
    -> QualifyNodeAccumulator
    -> QualifyNodeAccumulator
qualifyNode config currentDefName modRefs node acc =
    case node of
        Parser.Integer loc value ->
            { acc | qualifiedNodes = Ok (Integer loc value) :: acc.qualifiedNodes }

        Parser.Function loc value ->
            let
                qualifiedName =
                    qualifyName config value
            in
            if Dict.member value config.ast.functions then
                { acc | qualifiedNodes = Ok (Word loc qualifiedName) :: acc.qualifiedNodes }

            else
                case Dict.get value builtinDict of
                    Just builtin ->
                        { acc | qualifiedNodes = Ok (Builtin loc builtin) :: acc.qualifiedNodes }

                    Nothing ->
                        case resolveImportedWord config modRefs value of
                            Nothing ->
                                { acc | qualifiedNodes = Err (UnknownWordRef loc value) :: acc.qualifiedNodes }

                            Just mod ->
                                if String.startsWith "/" mod then
                                    let
                                        path =
                                            mod
                                                |> String.split "/"
                                                |> List.drop 3
                                    in
                                    qualifyNode
                                        config
                                        currentDefName
                                        modRefs
                                        (Parser.ExternalFunction loc path value)
                                        acc

                                else
                                    let
                                        path =
                                            String.split "/" mod
                                    in
                                    qualifyNode
                                        config
                                        currentDefName
                                        modRefs
                                        (Parser.PackageFunction loc path value)
                                        acc

        Parser.PackageFunction loc path value ->
            let
                normalizedPathPreAliasCheck =
                    String.join "/" path

                normalizedPath =
                    Dict.get normalizedPathPreAliasCheck modRefs.aliases
                        |> Maybe.withDefault normalizedPathPreAliasCheck
            in
            if String.startsWith "/" normalizedPath then
                let
                    externalWordNode =
                        Parser.ExternalFunction
                            loc
                            (List.drop 1 <| String.split "/" normalizedPath)
                            value
                in
                qualifyNode config currentDefName modRefs externalWordNode acc

            else
                let
                    qualifiedPath =
                        qualifyPackageModule config.packageName normalizedPath

                    qualifiedName =
                        String.join "/" [ qualifiedPath, value ]

                    _ =
                        Dict.keys config.inProgressAST.words
                in
                case Dict.get qualifiedName config.inProgressAST.words of
                    Nothing ->
                        { acc | qualifiedNodes = Err (UnknownWordRef loc qualifiedName) :: acc.qualifiedNodes }

                    Just word ->
                        if word.metadata.isExposed then
                            { acc | qualifiedNodes = Ok (Word loc qualifiedName) :: acc.qualifiedNodes }

                        else
                            { acc | qualifiedNodes = Err (WordNotExposed loc qualifiedName) :: acc.qualifiedNodes }

        Parser.ExternalFunction loc path value ->
            let
                normalizedPath =
                    "/" ++ String.join "/" path
            in
            case Dict.get normalizedPath config.externalModules of
                Nothing ->
                    { acc | qualifiedNodes = Err (UnknownWordRef loc (normalizedPath ++ "/" ++ value)) :: acc.qualifiedNodes }

                Just package ->
                    let
                        fullReference =
                            String.concat
                                [ "/"
                                , package
                                , normalizedPath
                                , "/"
                                , value
                                ]
                    in
                    case Dict.get fullReference config.inProgressAST.words of
                        Nothing ->
                            { acc | qualifiedNodes = Err (UnknownWordRef loc fullReference) :: acc.qualifiedNodes }

                        Just def ->
                            if def.metadata.isExposed then
                                { acc | qualifiedNodes = Ok (Word loc fullReference) :: acc.qualifiedNodes }

                            else
                                { acc | qualifiedNodes = Err (WordNotExposed loc fullReference) :: acc.qualifiedNodes }

        Parser.ConstructType typeName ->
            { acc | qualifiedNodes = Ok (ConstructType (qualifyName config typeName)) :: acc.qualifiedNodes }

        Parser.SetMember typeName memberName ->
            { acc | qualifiedNodes = Ok (SetMember (qualifyName config typeName) memberName) :: acc.qualifiedNodes }

        Parser.GetMember typeName memberName ->
            { acc | qualifiedNodes = Ok (GetMember (qualifyName config typeName) memberName) :: acc.qualifiedNodes }

        Parser.InlineFunction sourceLocation quotImpl ->
            let
                quoteName =
                    if String.startsWith "quote:" currentDefName then
                        currentDefName ++ "/" ++ String.fromInt acc.availableQuoteId

                    else
                        "quote:" ++ qualifyName config currentDefName ++ "/" ++ String.fromInt acc.availableQuoteId

                ( newWordsAfterQuot, qualifiedQuotImplResult ) =
                    initQualifyNode config quoteName modRefs acc.qualifiedWords quotImpl
            in
            case qualifiedQuotImplResult of
                Ok qualifiedQuotImpl ->
                    { acc
                        | availableQuoteId = acc.availableQuoteId + 1
                        , qualifiedWords =
                            Dict.insert quoteName
                                { name = quoteName
                                , metadata =
                                    Metadata.default
                                        |> Metadata.isQuoted
                                , implementation = SoloImpl qualifiedQuotImpl
                                }
                                newWordsAfterQuot
                        , qualifiedNodes = Ok (WordRef sourceLocation quoteName) :: acc.qualifiedNodes
                    }

                Err err ->
                    { acc | qualifiedNodes = Err err :: acc.qualifiedNodes }


typeDefinitionName : TypeDefinition -> String
typeDefinitionName typeDef =
    case typeDef of
        CustomTypeDef name _ _ _ _ ->
            name

        UnionTypeDef name _ _ _ _ ->
            name


qualifyName : RunConfig -> String -> String
qualifyName config name =
    if config.packageName == "" then
        name

    else
        String.concat
            [ "/"
            , config.packageName
            , "/"
            , config.modulePath
            , "/"
            , name
            ]


qualifyPackageModule : String -> String -> String
qualifyPackageModule packageName path =
    if packageName == "" then
        path

    else
        String.concat
            [ "/"
            , packageName
            , "/"
            , path
            ]


resolveImportedWord : RunConfig -> ModuleReferences -> String -> Maybe String
resolveImportedWord config modRefs name =
    let
        explicitImports =
            modRefs.imports
                |> Dict.toList
                |> List.find (\( _, v ) -> List.member name v)
                |> Maybe.map Tuple.first
                |> Maybe.andThen resolveMod

        potentialCandidates =
            modRefs.imports
                |> Dict.filter (\_ v -> List.isEmpty v)
                |> Dict.keys
                |> List.filterMap resolveMod
                |> List.map (\mod -> ( mod, mod ++ "/" ++ name ))

        resolveMod mod =
            if String.startsWith "/" mod then
                Dict.get mod config.externalModules
                    |> Maybe.map
                        (\package ->
                            qualifyPackageModule package (String.dropLeft 1 mod)
                        )

            else
                Just <| qualifyPackageModule config.packageName mod
    in
    case explicitImports of
        Just _ ->
            explicitImports

        Nothing ->
            potentialCandidates
                |> List.map (\( mod, qName ) -> ( mod, Dict.get qName config.inProgressAST.words ))
                |> List.filter (\( _, possibleDef ) -> possibleDef /= Nothing)
                |> List.head
                |> Maybe.map Tuple.first


resolveImportedType : RunConfig -> ModuleReferences -> String -> Maybe String
resolveImportedType config modRefs name =
    let
        explicitImports =
            modRefs.imports
                |> Dict.toList
                |> List.find (\( _, v ) -> List.member name v)
                |> Maybe.map Tuple.first
                |> Maybe.andThen resolveMod

        potentialCandidates =
            modRefs.imports
                |> Dict.filter (\_ v -> List.isEmpty v)
                |> Dict.keys
                |> List.filterMap resolveMod
                |> List.map (\mod -> ( mod, mod ++ "/" ++ name ))

        resolveMod mod =
            if String.startsWith "/" mod then
                Dict.get mod config.externalModules
                    |> Maybe.map
                        (\package ->
                            String.dropLeft 1 mod
                                |> qualifyPackageModule package
                        )

            else
                Just <| qualifyPackageModule config.packageName mod
    in
    case explicitImports of
        Just _ ->
            explicitImports

        Nothing ->
            potentialCandidates
                |> List.map (\( mod, qName ) -> ( mod, Dict.get qName config.inProgressAST.types ))
                |> List.filter (\( _, possibleDef ) -> possibleDef /= Nothing)
                |> List.head
                |> Maybe.map Tuple.first



-- Dependant modules


type alias RequiredModulesConfig =
    { packageName : String
    , ast : Parser.AST
    , externalModules : Dict String String
    }


requiredModules : RequiredModulesConfig -> Set String
requiredModules config =
    let
        modDef =
            moduleDefinition config

        topLevelAliases =
            modDef.aliases

        topLevelAliasTargets =
            topLevelAliases
                |> Dict.values
                |> Set.fromList

        topLevelImports =
            modDef.imports
                |> Dict.keys
                |> Set.fromList

        typeRequirements =
            config.ast.types
                |> Dict.foldl
                    (\_ t acc -> Set.union (requiredModulesOfType t) acc)
                    Set.empty

        wordRequirements =
            config.ast.functions
                |> Dict.foldl
                    (\_ w acc -> Set.union (requiredModulesOfWord topLevelAliases w) acc)
                    Set.empty

        fullyQualify mod acc =
            if String.startsWith "/" mod then
                case Dict.get mod config.externalModules of
                    Just package ->
                        Set.insert
                            (String.concat
                                [ "/"
                                , package
                                , mod
                                ]
                            )
                            acc

                    Nothing ->
                        acc

            else
                Set.insert (qualifyPackageModule config.packageName mod) acc
    in
    topLevelAliasTargets
        |> Set.union topLevelImports
        |> Set.union typeRequirements
        |> Set.union wordRequirements
        |> Set.foldl fullyQualify Set.empty


requiredModulesOfType : Parser.TypeDefinition -> Set String
requiredModulesOfType typeDef =
    case typeDef of
        Parser.CustomTypeDef _ _ _ members ->
            members
                |> List.map Tuple.second
                |> List.filterMap extractModuleReferenceFromType
                |> Set.fromList

        Parser.UnionTypeDef _ _ _ members ->
            members
                |> List.filterMap extractModuleReferenceFromType
                |> Set.fromList


requiredModulesOfWord : Dict String String -> Parser.FunctionDefinition -> Set String
requiredModulesOfWord topLevelAliases word =
    let
        wordAliases =
            word.aliases
                |> Dict.values
                |> Set.fromList

        wordImports =
            word.imports
                |> Dict.keys
                |> Set.fromList

        typeSignature =
            case word.typeSignature of
                Parser.NotProvided ->
                    Set.empty

                Parser.UserProvided wordType ->
                    moduleReferenceFromWordType wordType

                Parser.Verified wordType ->
                    moduleReferenceFromWordType wordType

        moduleReferenceFromWordType wordType =
            wordType.input
                ++ wordType.output
                |> List.filterMap extractModuleReferenceFromType
                |> Set.fromList

        matches =
            case word.implementation of
                Parser.SoloImpl _ ->
                    Set.empty

                Parser.MultiImpl branches _ ->
                    branches
                        |> List.map Tuple.first
                        |> List.map extractMatchType
                        |> List.filterMap extractModuleReferenceFromType
                        |> Set.fromList

        extractMatchType (Parser.TypeMatch _ tipe _) =
            tipe

        impls =
            case word.implementation of
                Parser.SoloImpl impl ->
                    [ impl ]

                Parser.MultiImpl branches impl ->
                    impl :: List.map Tuple.second branches

        wordReferences =
            impls
                |> List.concat
                |> List.filterMap (extractModuleReferenceFromNode topLevelAliases word)
                |> Set.fromList
    in
    wordAliases
        |> Set.union wordImports
        |> Set.union typeSignature
        |> Set.union matches
        |> Set.union wordReferences


extractModuleReferenceFromType : Parser.PossiblyQualifiedType -> Maybe String
extractModuleReferenceFromType ref =
    case ref of
        Parser.ExternalRef path _ _ ->
            Just <| "/" ++ String.join "/" path

        Parser.InternalRef path _ _ ->
            Just <| String.join "/" path

        _ ->
            Nothing


extractModuleReferenceFromNode : Dict String String -> Parser.FunctionDefinition -> Parser.AstNode -> Maybe String
extractModuleReferenceFromNode topLevelAliases meta node =
    case node of
        Parser.PackageFunction _ [ potentialAlias ] _ ->
            case
                ( Dict.get potentialAlias topLevelAliases
                , Dict.get potentialAlias meta.aliases
                )
            of
                ( Just _, _ ) ->
                    Nothing

                ( _, Just _ ) ->
                    Nothing

                ( Nothing, Nothing ) ->
                    Just potentialAlias

        Parser.PackageFunction _ path _ ->
            Just (String.join "/" path)

        Parser.ExternalFunction _ path _ ->
            Just ("/" ++ String.join "/" path)

        _ ->
            Nothing
