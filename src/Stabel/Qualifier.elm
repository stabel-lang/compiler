module Stabel.Qualifier exposing
    ( AST
    , FunctionDefinition
    , FunctionImplementation(..)
    , Node(..)
    , TypeDefinition
    , TypeDefinitionMembers(..)
    , TypeMatch(..)
    , TypeMatchValue(..)
    , requiredModules
    , run
    )

import Dict exposing (Dict)
import List.Extra as List
import Result.Extra as Result
import Set exposing (Set)
import Stabel.Data.Builtin as Builtin exposing (Builtin)
import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Stabel.Data.Type as Type exposing (FunctionType, Type)
import Stabel.Data.TypeSignature as TypeSignature exposing (TypeSignature)
import Stabel.Parser as Parser
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.Type as Parser
import Stabel.Qualifier.Problem exposing (Problem(..))


type alias AST =
    { types : Dict String TypeDefinition
    , functions : Dict String FunctionDefinition
    }


type alias TypeDefinition =
    { name : String
    , exposed : Bool
    , sourceLocation : SourceLocationRange
    , generics : List String
    , members : TypeDefinitionMembers
    }


type TypeDefinitionMembers
    = StructMembers (List ( String, Type ))
    | UnionMembers (List Type)


type alias FunctionDefinition =
    { name : String
    , sourceLocation : Maybe SourceLocationRange
    , typeSignature : TypeSignature
    , exposed : Bool
    , inline : Bool
    , implementation : FunctionImplementation
    }


type FunctionImplementation
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
    | Function SourceLocationRange FunctionDefinition
    | FunctionRef SourceLocationRange FunctionDefinition
    | ConstructType TypeDefinition
    | GetMember TypeDefinition String Type
    | SetMember TypeDefinition String Type
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
            Dict.foldl
                (\_ val acc -> qualifyType config val acc)
                ( [], Dict.empty )
                config.ast.types
                |> Tuple.mapSecond (\qt -> Dict.map (\_ v -> resolveUnionInTypeDefs qt v) qt)

        allQualifiedTypes =
            Dict.union qualifiedTypes config.inProgressAST.types

        ( functionErrors, qualifiedFunctions, inlineFunctionNames ) =
            Dict.foldl
                (\_ val acc -> qualifyDefinition config allQualifiedTypes val acc)
                ( [], Dict.empty, Set.empty )
                config.ast.functions
    in
    case ( typeErrors, functionErrors ) of
        ( [], [] ) ->
            Ok
                { types = qualifiedTypes
                , functions = qualifiedFunctions
                , referenceableFunctions = inlineFunctionNames
                }

        _ ->
            Err <| typeErrors ++ functionErrors


type alias ModuleDefinitionConfig a =
    { a
        | ast : Parser.AST
        , externalModules : Dict String String
    }


moduleDefinition : ModuleDefinitionConfig a -> ModuleDefinition.Definition
moduleDefinition config =
    let
        defaultImports =
            if Dict.get "/core" config.externalModules == Just "stabel/standard_library" then
                Dict.fromList [ ( "/core", [] ) ]

            else
                Dict.empty

        def =
            ModuleDefinition.definition config.ast.moduleDefinition
    in
    { imports = Dict.union def.imports defaultImports
    , aliases = def.aliases
    , exposes = def.exposes
    }


resolveUnionInTypeDefs : Dict String TypeDefinition -> TypeDefinition -> TypeDefinition
resolveUnionInTypeDefs qt td =
    { td
        | members =
            case td.members of
                StructMembers members ->
                    StructMembers (List.map (Tuple.mapSecond (resolveUnion qt)) members)

                UnionMembers members ->
                    UnionMembers (List.map (resolveUnion qt) members)
    }


resolveUnion : Dict String TypeDefinition -> Type -> Type
resolveUnion typeDefs type_ =
    case type_ of
        Type.Custom typeName ->
            case Maybe.map .members (Dict.get typeName typeDefs) of
                Just (UnionMembers members) ->
                    Type.Union (Just typeName) members

                _ ->
                    type_

        Type.CustomGeneric typeName types ->
            case Dict.get typeName typeDefs of
                Just result ->
                    case result.members of
                        UnionMembers members ->
                            let
                                genericsMap =
                                    List.map2 Tuple.pair result.generics types
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
                            Type.Union
                                (Just typeName)
                                (List.map rebindGenerics members)

                        _ ->
                            type_

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

        qualifiedName =
            qualifyName config typeDef.name

        qualifiedRange =
            SourceLocationRange
                config.ast.sourceReference
                typeDef.sourceLocation.start
                typeDef.sourceLocation.end

        exposed =
            Set.isEmpty exposes || Set.member typeDef.name exposes
    in
    case typeDef.members of
        Parser.StructMembers members ->
            let
                qualifiedMemberResult =
                    List.map (Tuple.mapSecond (qualifyMemberType config modRefs qualifiedRange)) members
                        |> List.map raiseTupleError
                        |> Result.combine

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
                        { name = qualifiedName
                        , exposed = exposed
                        , sourceLocation = qualifiedRange
                        , generics = typeDef.generics
                        , members = StructMembers qualifiedMembers
                        }
                        acc
                    )

        Parser.UnionMembers memberTypes ->
            let
                qualifiedMemberTypesResult =
                    List.map (qualifyMemberType config modRefs qualifiedRange) memberTypes
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
                        { name = qualifiedName
                        , exposed = exposed
                        , sourceLocation = qualifiedRange
                        , generics = typeDef.generics
                        , members = UnionMembers qualifiedMemberTypes
                        }
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
            in
            refLookup qualifiedName binds

        refLookup name binds =
            let
                bindResult =
                    binds
                        |> List.map (qualifyMemberType config modRefs range)
                        |> Result.combine

                maybeType =
                    Dict.get name config.inProgressAST.types

                maybeMembers =
                    Maybe.map .members maybeType

                exposed =
                    maybeType
                        |> Maybe.map .exposed
                        |> Maybe.withDefault False
            in
            case ( exposed, maybeMembers, bindResult ) of
                ( _, Nothing, _ ) ->
                    Err <| UnknownTypeRef range name

                ( False, _, _ ) ->
                    Err <| TypeNotExposed range name

                ( _, _, Err err ) ->
                    Err err

                ( True, Just (StructMembers _), Ok [] ) ->
                    Ok <| Type.Custom name

                ( True, Just (StructMembers _), Ok qualifiedBinds ) ->
                    Ok <| Type.CustomGeneric name qualifiedBinds

                ( True, Just (UnionMembers members), _ ) ->
                    Ok <| Type.Union (Just name) members

        importsLookup name binds =
            case resolveImportedType config modRefs name of
                Just importedModule ->
                    if representsExternalModule importedModule then
                        let
                            nextPath =
                                splitExternalPackagePath importedModule
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
                    if representsExternalModule val then
                        let
                            newPath =
                                splitExternalPackagePath val
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
            in
            refLookup qualifiedName binds

        Parser.Generic sym ->
            Ok (Type.Generic sym)

        Parser.FunctionType sign ->
            let
                inputResult =
                    sign.input
                        |> List.map (qualifyFunctionType config modRefs range)
                        |> Result.combine

                outputResult =
                    sign.output
                        |> List.map (qualifyFunctionType config modRefs range)
                        |> Result.combine
            in
            case ( inputResult, outputResult ) of
                ( Ok input, Ok output ) ->
                    Ok <|
                        Type.FunctionSignature
                            { input = input
                            , output = output
                            }

                ( Err input, _ ) ->
                    Err input

                ( _, Err output ) ->
                    Err output


qualifyFunctionType :
    RunConfig
    -> ModuleReferences
    -> SourceLocationRange
    -> Parser.PossiblyQualifiedTypeOrStackRange
    -> Result Problem Type
qualifyFunctionType config modRefs range type_ =
    case type_ of
        Parser.StackRange sym ->
            Ok (Type.StackRange sym)

        Parser.NotStackRange pqt ->
            qualifyMemberType config modRefs range pqt


qualifyDefinition :
    RunConfig
    -> Dict String TypeDefinition
    -> Parser.FunctionDefinition
    -> ( List Problem, Dict String FunctionDefinition, Set String )
    -> ( List Problem, Dict String FunctionDefinition, Set String )
qualifyDefinition config qualifiedTypes unqualifiedFunction ( errors, acc, inlineFuncs ) =
    let
        ( whens, impl ) =
            case unqualifiedFunction.implementation of
                Parser.SoloImpl defImpl ->
                    ( [], defImpl )

                Parser.MultiImpl whenImpl defImpl ->
                    ( whenImpl, defImpl )

        modDef =
            moduleDefinition config

        moduleReferences =
            { aliases = Dict.union unqualifiedFunction.aliases modDef.aliases
            , imports = Dict.union unqualifiedFunction.imports modDef.imports
            }

        ( newFunctionsAfterWhens, inlineFunctionNamesAfterWhens, qualifiedWhensResult ) =
            whens
                |> List.foldr
                    (qualifyWhen
                        config
                        qualifiedTypes
                        unqualifiedFunction.name
                        moduleReferences
                    )
                    ( acc, Set.empty, [] )
                |> (\( a, b, c ) -> ( a, b, Result.combine c ))

        implQualifyResult =
            initQualifyNode config unqualifiedFunction.name moduleReferences newFunctionsAfterWhens impl

        qualifiedMetadataResult =
            qualifyMetadata config qualifiedTypes unqualifiedFunction

        qualifiedName =
            qualifyName config unqualifiedFunction.name

        mapLoc loc =
            SourceLocationRange
                config.ast.sourceReference
                loc.start
                loc.end

        newInlineFuncs =
            inlineFuncs
                |> Set.union inlineFunctionNamesAfterWhens
                |> Set.union implQualifyResult.inlineFunctionNames
    in
    case ( qualifiedWhensResult, implQualifyResult.qualifiedNodes, qualifiedMetadataResult ) of
        ( Ok qualifiedWhens, Ok qualifiedImplementation, Ok ( typeSignature, exposed ) ) ->
            ( errors
            , Dict.insert qualifiedName
                { name = qualifiedName
                , sourceLocation = Maybe.map mapLoc unqualifiedFunction.sourceLocationRange
                , typeSignature = typeSignature
                , exposed = exposed
                , implementation =
                    if List.isEmpty qualifiedWhens then
                        SoloImpl qualifiedImplementation

                    else
                        MultiImpl qualifiedWhens qualifiedImplementation
                }
                implQualifyResult.qualifiedFunctions
            , newInlineFuncs
            )

        ( Err whenError, _, _ ) ->
            ( whenError :: errors
            , implQualifyResult.qualifiedFunctions
            , newInlineFuncs
            )

        ( _, Err implError, _ ) ->
            ( implError :: errors
            , implQualifyResult.qualifiedFunctions
            , newInlineFuncs
            )

        ( _, _, Err metaError ) ->
            ( metaError :: errors
            , implQualifyResult.qualifiedFunctions
            , newInlineFuncs
            )


qualifyMetadata :
    RunConfig
    -> Dict String TypeDefinition
    -> Parser.FunctionDefinition
    -> Result Problem ( TypeSignature, Bool )
qualifyMetadata config qualifiedTypes function =
    let
        functionRange =
            function.sourceLocationRange
                |> Maybe.map
                    (\r ->
                        SourceLocationRange
                            config.ast.sourceReference
                            r.start
                            r.end
                    )
                |> Maybe.withDefault SourceLocation.emptyRange

        inputLength =
            function.typeSignature
                |> AssociatedFunctionSignature.toMaybe
                |> Maybe.map (.input >> List.length)
                |> Maybe.withDefault 0

        modDef =
            moduleDefinition config

        modRefs =
            { aliases = Dict.union modDef.aliases function.aliases
            , imports = Dict.union modDef.imports function.imports
            }
    in
    AssociatedFunctionSignature.toMaybe function.typeSignature
        |> Maybe.map (\ts -> ts.input ++ ts.output)
        |> Maybe.withDefault []
        |> List.map (qualifyFunctionType config modRefs functionRange)
        |> Result.combine
        |> Result.map
            (\qualifiedFlatTypeSignature ->
                let
                    functionType =
                        { input = List.take inputLength qualifiedFlatTypeSignature
                        , output = List.drop inputLength qualifiedFlatTypeSignature
                        }

                    ts =
                        case function.typeSignature of
                            AssociatedFunctionSignature.NotProvided ->
                                TypeSignature.NotProvided

                            AssociatedFunctionSignature.UserProvided _ ->
                                TypeSignature.UserProvided functionType

                            AssociatedFunctionSignature.Verified _ ->
                                TypeSignature.CompilerProvided functionType
                in
                ( TypeSignature.map (resolveUnions qualifiedTypes) ts
                , case config.ast.moduleDefinition of
                    ModuleDefinition.Undefined ->
                        True

                    ModuleDefinition.Defined def ->
                        Set.member function.name def.exposes
                )
            )


resolveUnions : Dict String TypeDefinition -> FunctionType -> FunctionType
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
    -> ( Dict String FunctionDefinition, Set String, List (Result Problem ( TypeMatch, List Node )) )
    -> ( Dict String FunctionDefinition, Set String, List (Result Problem ( TypeMatch, List Node )) )
qualifyWhen config qualifiedTypes functionName modRefs ( typeMatch, impl ) ( qualifiedFunctions, inlineFunctionNames, result ) =
    let
        qualifyNodeResult =
            initQualifyNode config functionName modRefs qualifiedFunctions impl

        qualifiedMatchResult =
            qualifyMatch config qualifiedTypes modRefs typeMatch
    in
    ( qualifyNodeResult.qualifiedFunctions
    , Set.union inlineFunctionNames qualifyNodeResult.inlineFunctionNames
    , case ( qualifyNodeResult.qualifiedNodes, qualifiedMatchResult ) of
        ( Err err, _ ) ->
            Err err :: result

        ( _, Err err ) ->
            Err err :: result

        ( Ok qualifiedImplementation, Ok qualifiedMatch ) ->
            Ok ( qualifiedMatch, qualifiedImplementation ) :: result
    )


qualifyMatch :
    RunConfig
    -> Dict String TypeDefinition
    -> ModuleReferences
    -> Parser.TypeMatch
    -> Result Problem TypeMatch
qualifyMatch config qualifiedTypes modRefs typeMatch =
    let
        qualifiedRange range =
            SourceLocationRange
                config.ast.sourceReference
                range.start
                range.end

        qualifiedNameToMatch range name patterns =
            case Dict.get name qualifiedTypes of
                Just typeDef ->
                    if not typeDef.exposed then
                        Err <| TypeNotExposed range name

                    else
                        case typeDef.members of
                            StructMembers members ->
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
                                        case typeDef.generics of
                                            [] ->
                                                Type.Custom name

                                            _ ->
                                                Type.CustomGeneric
                                                    name
                                                    (List.map Type.Generic typeDef.generics)
                                in
                                case qualifiedPatternsResult of
                                    Ok qualifiedPatterns ->
                                        Ok <| TypeMatch range actualType qualifiedPatterns

                                    Err err ->
                                        Err err

                            UnionMembers types ->
                                if List.isEmpty patterns then
                                    Ok <| TypeMatch range (Type.Union (Just name) types) []

                                else
                                    Err <| UnionTypeMatchWithPatterns range

                Nothing ->
                    Err <| UnknownTypeRef range name
    in
    case typeMatch of
        Parser.TypeMatch range (Parser.LocalRef "Int" []) [] ->
            Ok <| TypeMatch (qualifiedRange range) Type.Int []

        Parser.TypeMatch range (Parser.LocalRef "Int" []) [ ( "value", Parser.LiteralInt val ) ] ->
            Ok <| TypeMatch (qualifiedRange range) Type.Int [ ( "value", LiteralInt val ) ]

        Parser.TypeMatch range (Parser.Generic sym) [] ->
            Ok <| TypeMatch (qualifiedRange range) (Type.Generic sym) []

        Parser.TypeMatch range (Parser.LocalRef name []) patterns ->
            case qualifiedNameToMatch (qualifiedRange range) (qualifyName config name) patterns of
                (Err (UnknownTypeRef _ _)) as errMsg ->
                    case resolveImportedType config modRefs name of
                        Just importedModule ->
                            qualifiedNameToMatch
                                (qualifiedRange range)
                                (importedModule ++ "/" ++ name)
                                patterns

                        Nothing ->
                            errMsg

                result ->
                    result

        Parser.TypeMatch range (Parser.InternalRef [ possibleAlias ] name _) patterns ->
            case Dict.get possibleAlias modRefs.aliases of
                Just actualPath ->
                    if representsExternalModule actualPath then
                        let
                            extPath =
                                splitExternalPackagePath actualPath
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
                        qualifiedNameToMatch (qualifiedRange range) qualifiedName patterns

                Nothing ->
                    let
                        qualifiedName =
                            possibleAlias
                                ++ "/"
                                ++ name
                                |> qualifyPackageModule config.packageName
                    in
                    qualifiedNameToMatch (qualifiedRange range) qualifiedName patterns

        Parser.TypeMatch range (Parser.InternalRef path name _) patterns ->
            let
                qualifiedName =
                    path
                        ++ [ name ]
                        |> String.join "/"
                        |> qualifyPackageModule config.packageName
            in
            qualifiedNameToMatch (qualifiedRange range) qualifiedName patterns

        Parser.TypeMatch range (Parser.ExternalRef path name _) patterns ->
            let
                pathString =
                    "/" ++ String.join "/" path

                qualifiedName =
                    Dict.get pathString config.externalModules
                        |> Maybe.map (\prefix -> "/" ++ prefix ++ pathString ++ "/" ++ name)
                        |> Maybe.withDefault ""
            in
            qualifiedNameToMatch (qualifiedRange range) qualifiedName patterns

        Parser.TypeMatch range _ _ ->
            Err <| InvalidTypeMatch (qualifiedRange range)


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
                type_
                    |> qualifyMemberType config modRefs range
                    |> Result.map (\qualifiedType -> ( fieldName, LiteralType qualifiedType ))

            Parser.RecursiveMatch typeMatch ->
                typeMatch
                    |> qualifyMatch config qualifiedTypes modRefs
                    |> Result.map (\match -> ( fieldName, RecursiveMatch match ))

    else
        Err <| NoSuchMemberOnType range typeName fieldName


initQualifyNode :
    RunConfig
    -> String
    -> ModuleReferences
    -> Dict String FunctionDefinition
    -> List Parser.AstNode
    -> QualifyNodeResult
initQualifyNode config currentDefName modRefs qualifiedFunctions impl =
    List.foldr
        (qualifyNode config currentDefName modRefs)
        (initQualifyNodeAccumulator qualifiedFunctions)
        impl
        |> (\acc ->
                { qualifiedFunctions = acc.qualifiedFunctions
                , qualifiedNodes = Result.combine acc.qualifiedNodes
                , inlineFunctionNames = acc.inlineFunctionNames
                }
           )


type alias QualifyNodeAccumulator =
    { availableInlineFuncId : Int
    , qualifiedFunctions : Dict String FunctionDefinition
    , qualifiedNodes : List (Result Problem Node)
    , inlineFunctionNames : Set String
    }


type alias QualifyNodeResult =
    { qualifiedFunctions : Dict String FunctionDefinition
    , qualifiedNodes : Result Problem (List Node)
    , inlineFunctionNames : Set String
    }


initQualifyNodeAccumulator : Dict String FunctionDefinition -> QualifyNodeAccumulator
initQualifyNodeAccumulator qualifiedFunctions =
    { availableInlineFuncId = 1
    , qualifiedFunctions = qualifiedFunctions
    , qualifiedNodes = []
    , inlineFunctionNames = Set.empty
    }


qualifyNode :
    RunConfig
    -> String
    -> ModuleReferences
    -> Parser.AstNode
    -> QualifyNodeAccumulator
    -> QualifyNodeAccumulator
qualifyNode config currentDefName modRefs node acc =
    let
        mapLoc loc =
            SourceLocationRange
                config.ast.sourceReference
                loc.start
                loc.end
    in
    case node of
        Parser.Integer loc value ->
            { acc
                | qualifiedNodes =
                    Ok (Integer (mapLoc loc) value)
                        :: acc.qualifiedNodes
            }

        Parser.Function loc value ->
            let
                qualifiedName =
                    qualifyName config value

                qLoc =
                    mapLoc loc
            in
            case Dict.get value config.ast.functions of
                Just func ->
                    { acc | qualifiedNodes = Ok (Function qLoc func) :: acc.qualifiedNodes }

                Nothing ->
                    case Dict.get value builtinDict of
                        Just builtin ->
                            { acc | qualifiedNodes = Ok (Builtin qLoc builtin) :: acc.qualifiedNodes }

                        Nothing ->
                            case resolveImportedFunction config modRefs value of
                                Nothing ->
                                    { acc | qualifiedNodes = Err (UnknownFunctionRef qLoc value) :: acc.qualifiedNodes }

                                Just mod ->
                                    if representsExternalModule mod then
                                        let
                                            path =
                                                splitExternalPackagePath mod
                                                    -- drop author/package
                                                    |> List.drop 2
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
                                                splitInternalPackagePath mod
                                        in
                                        qualifyNode
                                            config
                                            currentDefName
                                            modRefs
                                            (Parser.PackageFunction loc path value)
                                            acc

        Parser.PackageFunction loc path value ->
            let
                qLoc =
                    mapLoc loc

                normalizedPathPreAliasCheck =
                    String.join "/" path

                normalizedPath =
                    Dict.get normalizedPathPreAliasCheck modRefs.aliases
                        |> Maybe.withDefault normalizedPathPreAliasCheck
            in
            if representsExternalModule normalizedPath then
                let
                    externalFunctionNode =
                        Parser.ExternalFunction
                            loc
                            (splitExternalPackagePath normalizedPath)
                            value
                in
                qualifyNode config currentDefName modRefs externalFunctionNode acc

            else
                let
                    qualifiedPath =
                        qualifyPackageModule config.packageName normalizedPath

                    qualifiedName =
                        String.join "/" [ qualifiedPath, value ]
                in
                case Dict.get qualifiedName config.inProgressAST.functions of
                    Nothing ->
                        { acc | qualifiedNodes = Err (UnknownFunctionRef qLoc qualifiedName) :: acc.qualifiedNodes }

                    Just function ->
                        if function.exposed then
                            { acc | qualifiedNodes = Ok (Function qLoc qualifiedName) :: acc.qualifiedNodes }

                        else
                            { acc | qualifiedNodes = Err (FunctionNotExposed qLoc qualifiedName) :: acc.qualifiedNodes }

        Parser.ExternalFunction loc path value ->
            let
                qLoc =
                    mapLoc loc

                normalizedPath =
                    "/" ++ String.join "/" path
            in
            case Dict.get normalizedPath config.externalModules of
                Nothing ->
                    { acc | qualifiedNodes = Err (UnknownFunctionRef qLoc (normalizedPath ++ "/" ++ value)) :: acc.qualifiedNodes }

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
                    case Dict.get fullReference config.inProgressAST.functions of
                        Nothing ->
                            { acc | qualifiedNodes = Err (UnknownFunctionRef qLoc fullReference) :: acc.qualifiedNodes }

                        Just def ->
                            if def.exposed then
                                { acc | qualifiedNodes = Ok (Function qLoc fullReference) :: acc.qualifiedNodes }

                            else
                                { acc | qualifiedNodes = Err (FunctionNotExposed qLoc fullReference) :: acc.qualifiedNodes }

        Parser.ConstructType typeName ->
            { acc | qualifiedNodes = Ok (ConstructType (qualifyName config typeName)) :: acc.qualifiedNodes }

        Parser.SetMember typeName memberName ->
            { acc | qualifiedNodes = Ok (SetMember (qualifyName config typeName) memberName) :: acc.qualifiedNodes }

        Parser.GetMember typeName memberName ->
            { acc | qualifiedNodes = Ok (GetMember (qualifyName config typeName) memberName) :: acc.qualifiedNodes }

        Parser.InlineFunction sourceLocation quotImpl ->
            let
                inlineFuncName =
                    if String.startsWith "inlinefn:" currentDefName then
                        currentDefName ++ "/" ++ String.fromInt acc.availableInlineFuncId

                    else
                        "inlinefn:" ++ qualifyName config currentDefName ++ "/" ++ String.fromInt acc.availableInlineFuncId

                qualifyNodeResult =
                    initQualifyNode config inlineFuncName modRefs acc.qualifiedFunctions quotImpl
            in
            case qualifyNodeResult.qualifiedNodes of
                Ok [ Function _ qualifiedName ] ->
                    { acc
                        | qualifiedNodes =
                            Ok (FunctionRef (mapLoc sourceLocation) qualifiedName)
                                :: acc.qualifiedNodes
                        , qualifiedFunctions =
                            Dict.union
                                acc.qualifiedFunctions
                                qualifyNodeResult.qualifiedFunctions
                        , inlineFunctionNames =
                            acc.inlineFunctionNames
                                |> Set.union qualifyNodeResult.inlineFunctionNames
                                |> Set.insert qualifiedName
                    }

                Ok qualifiedQuotImpl ->
                    { acc
                        | availableInlineFuncId =
                            acc.availableInlineFuncId + 1
                        , qualifiedFunctions =
                            Dict.insert inlineFuncName
                                { name = inlineFuncName
                                , sourceLocation = Nothing
                                , typeSignature = TypeSignature.NotProvided
                                , exposed = False
                                , implementation = SoloImpl qualifiedQuotImpl
                                }
                                (Dict.union
                                    acc.qualifiedFunctions
                                    qualifyNodeResult.qualifiedFunctions
                                )
                        , qualifiedNodes =
                            Ok (FunctionRef (mapLoc sourceLocation) inlineFuncName)
                                :: acc.qualifiedNodes
                        , inlineFunctionNames =
                            acc.inlineFunctionNames
                                |> Set.union qualifyNodeResult.inlineFunctionNames
                                |> Set.insert inlineFuncName
                    }

                Err err ->
                    { acc | qualifiedNodes = Err err :: acc.qualifiedNodes }


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


resolveImportedFunction : RunConfig -> ModuleReferences -> String -> Maybe String
resolveImportedFunction config modRefs name =
    resolveImported config modRefs config.inProgressAST.functions name


resolveImportedType : RunConfig -> ModuleReferences -> String -> Maybe String
resolveImportedType config modRefs name =
    resolveImported config modRefs config.inProgressAST.types name


resolveImported : RunConfig -> ModuleReferences -> Dict String a -> String -> Maybe String
resolveImported config modRefs lookupTable name =
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
            if representsExternalModule mod then
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
                |> List.filterMap
                    (\( mod, qName ) ->
                        Dict.get qName lookupTable
                            |> Maybe.map (always mod)
                    )
                |> List.head


representsExternalModule : String -> Bool
representsExternalModule path =
    String.startsWith "/" path


splitExternalPackagePath : String -> List String
splitExternalPackagePath path =
    path
        |> String.split "/"
        -- Due to leading /
        |> List.drop 1


splitInternalPackagePath : String -> List String
splitInternalPackagePath =
    String.split "/"



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

        functionRequirements =
            config.ast.functions
                |> Dict.foldl
                    (\_ w acc -> Set.union (requiredModulesOfFunction topLevelAliases w) acc)
                    Set.empty

        fullyQualify mod acc =
            if representsExternalModule mod then
                case Dict.get mod config.externalModules of
                    Just package ->
                        Set.insert (String.concat [ "/", package, mod ]) acc

                    Nothing ->
                        acc

            else
                Set.insert (qualifyPackageModule config.packageName mod) acc
    in
    topLevelAliasTargets
        |> Set.union topLevelImports
        |> Set.union typeRequirements
        |> Set.union functionRequirements
        |> Set.foldl fullyQualify Set.empty


requiredModulesOfType : Parser.TypeDefinition -> Set String
requiredModulesOfType typeDef =
    case typeDef.members of
        Parser.StructMembers members ->
            members
                |> List.map Tuple.second
                |> List.filterMap extractModuleReferenceFromType
                |> Set.fromList

        Parser.UnionMembers members ->
            members
                |> List.filterMap extractModuleReferenceFromType
                |> Set.fromList


requiredModulesOfFunction : Dict String String -> Parser.FunctionDefinition -> Set String
requiredModulesOfFunction topLevelAliases function =
    let
        functionAliases =
            function.aliases
                |> Dict.values
                |> Set.fromList

        functionImports =
            function.imports
                |> Dict.keys
                |> Set.fromList

        typeSignature =
            function.typeSignature
                |> AssociatedFunctionSignature.toMaybe
                |> Maybe.map moduleReferenceFromFunctionType
                |> Maybe.withDefault Set.empty

        moduleReferenceFromFunctionType functionType =
            functionType.input
                ++ functionType.output
                |> List.filterMap extractModuleReferenceFromFunctionType
                |> Set.fromList

        matches =
            case function.implementation of
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
            case function.implementation of
                Parser.SoloImpl impl ->
                    [ impl ]

                Parser.MultiImpl branches impl ->
                    impl :: List.map Tuple.second branches

        functionReferences =
            impls
                |> List.concat
                |> List.filterMap (extractModuleReferenceFromNode topLevelAliases function)
                |> Set.fromList
    in
    functionAliases
        |> Set.union functionImports
        |> Set.union typeSignature
        |> Set.union matches
        |> Set.union functionReferences


extractModuleReferenceFromFunctionType : Parser.PossiblyQualifiedTypeOrStackRange -> Maybe String
extractModuleReferenceFromFunctionType ref =
    case ref of
        Parser.StackRange _ ->
            Nothing

        Parser.NotStackRange t ->
            extractModuleReferenceFromType t


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
