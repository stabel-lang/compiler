module Test.PackageLoader exposing (suite)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Expect exposing (Expectation)
import List.Extra as List
import Set
import Stabel.Data.Builtin as Builtin
import Stabel.Data.PackagePath as PackagePath
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.TypeSignature as TypeSignature
import Stabel.PackageLoader as PackageLoader
import Stabel.Qualifier as Qualifier
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as Util


suite : Test
suite =
    let
        initOpts =
            { projectDirPath = "/project"
            , stdLibPath = "/project/lib/unused"
            }
    in
    describe "PackageLoader"
        [ test "Passes the load package metadata step" <|
            \_ ->
                PackageLoader.init initOpts
                    |> Expect.equal (PackageLoader.Initializing initOpts <| PackageLoader.ReadFile "/project" "stabel.json")
        , test "Retrieves necessary files" <|
            \_ ->
                PackageLoader.init initOpts
                    |> expectSideEffects testFiles
                        [ PackageLoader.ReadFile "/project" "stabel.json"
                        , PackageLoader.ReadFile "/project/lib/template_strings" "stabel.json"
                        , PackageLoader.ResolveDirectories "/project/lib/template_strings/lib"
                        , PackageLoader.ReadFile "/project/lib/template_strings/lib/version" "stabel.json"
                        , PackageLoader.ReadFile "/project/lib/unused" "stabel.json"
                        , PackageLoader.ReadFile "/project/lib/version" "stabel.json"
                        , PackageLoader.ResolvePackageModules "robheghan/fnv" "/project"
                        , PackageLoader.ResolvePackageModules "jarvis/template_strings" "/project/lib/template_strings"
                        , PackageLoader.ResolvePackageModules "stabel/version" "/project/lib/template_strings/lib/version"
                        , PackageLoader.ReadFile "/project/src" "mod1.stbl"
                        , PackageLoader.ReadFile "/project/lib/template_strings/lib/version/src/version" "data.stbl"
                        ]
        , test "Compiles project with external dependencies to qualified AST" <|
            \_ ->
                let
                    loaderResult =
                        PackageLoader.init initOpts
                            |> resolveSideEffects testFiles []
                            |> Result.map Tuple.second

                    numberDef =
                        { name = "/stabel/version/version/data/number"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature = TypeSignature.NotProvided
                        , implementation =
                            Qualifier.SoloImpl
                                [ Qualifier.Integer emptyRange 2
                                ]
                        }
                in
                case loaderResult of
                    Err msg ->
                        Expect.fail msg

                    Ok ast ->
                        Expect.equal
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "/robheghan/fnv/mod1/next-version"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            Qualifier.SoloImpl
                                                [ Qualifier.Function emptyRange numberDef
                                                , Qualifier.Integer emptyRange 1
                                                , Qualifier.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , numberDef
                                    ]
                            , referenceableFunctions = Set.empty
                            }
                            (Util.stripLocations ast)
        , test "Compiles project with multiple dependant modules to qualified AST" <|
            \_ ->
                let
                    loaderResult =
                        PackageLoader.init initOpts
                            |> resolveSideEffects testFilesInternalConsistency []
                            |> Result.map Tuple.second

                    bumpVersionDef =
                        { name = "/robheghan/dummy/mod1/bump-version"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature = TypeSignature.NotProvided
                        , implementation =
                            Qualifier.SoloImpl
                                [ Qualifier.Integer emptyRange 1
                                , Qualifier.Builtin emptyRange Builtin.Plus
                                ]
                        }

                    versionDef =
                        { name = "/robheghan/dummy/mod2/version"
                        , exposed = True
                        , sourceLocation = Nothing
                        , typeSignature = TypeSignature.NotProvided
                        , implementation =
                            Qualifier.SoloImpl
                                [ Qualifier.Integer emptyRange 5
                                ]
                        }
                in
                case loaderResult of
                    Err msg ->
                        Expect.fail msg

                    Ok ast ->
                        Expect.equal
                            { types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ versionDef
                                    , bumpVersionDef
                                    , { name = "/robheghan/dummy/mod3/next-version"
                                      , exposed = True
                                      , sourceLocation = Nothing
                                      , typeSignature = TypeSignature.NotProvided
                                      , implementation =
                                            Qualifier.SoloImpl
                                                [ Qualifier.Function emptyRange versionDef
                                                , Qualifier.Function emptyRange bumpVersionDef
                                                ]
                                      }
                                    ]
                            , referenceableFunctions = Set.empty
                            }
                            (Util.stripLocations ast)
        , describe "Errors" <|
            let
                load files =
                    PackageLoader.init initOpts
                        |> resolveSideEffects files []
                        |> Result.map Tuple.second
            in
            [ test "Exposed modules is respected" <|
                \_ ->
                    case load testFilesAccessUnexposedModule of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Did not expect qualification to succeed"
            , test "Exposed modules of transitive deps are not accessible" <|
                \_ ->
                    case load testFilesTransitiveDepBleed of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Did not expect qualification to succeed"
            , test "Transitive deps are loaded (make sure above test fails for correct reason)" <|
                \_ ->
                    case load testFilesTransitiveDep of
                        Err _ ->
                            Expect.fail "Did not expect qualification to fail"

                        Ok _ ->
                            Expect.pass
            , test "An external reference shouldn't match an internal module" <|
                \_ ->
                    case load testFilesExternalRef of
                        Err _ ->
                            Expect.fail "Did not expect failure"

                        Ok _ ->
                            Expect.pass
            ]
        ]


expectSideEffects : Dict String String -> List PackageLoader.SideEffect -> PackageLoader.Model -> Expectation
expectSideEffects fileSystem expectedSFs model =
    case resolveSideEffects fileSystem [] model of
        Err msg ->
            Expect.fail msg

        Ok ( seenSfs, _ ) ->
            let
                sfDiff =
                    List.filter (\sf -> not <| List.member sf expectedSFs) seenSfs

                sfDiffRev =
                    List.filter (\sf -> not <| List.member sf seenSfs) expectedSFs
            in
            Expect.equalLists [] (sfDiff ++ sfDiffRev)


resolveSideEffects :
    Dict String String
    -> List PackageLoader.SideEffect
    -> PackageLoader.Model
    -> Result String ( List PackageLoader.SideEffect, Qualifier.AST )
resolveSideEffects fileSystem seenSfs model =
    case PackageLoader.getSideEffect model of
        Nothing ->
            case model of
                PackageLoader.Done ast ->
                    Ok ( seenSfs, ast )

                _ ->
                    Err <| "Expected model be Done, was: " ++ Debug.toString model

        Just sideEffect ->
            case sideEffect of
                PackageLoader.ReadFile path filename ->
                    case Dict.get (path ++ "/" ++ filename) fileSystem of
                        Just fileContent ->
                            resolveSideEffects
                                fileSystem
                                (sideEffect :: seenSfs)
                                (PackageLoader.update
                                    (PackageLoader.FileContents path filename fileContent)
                                    model
                                )

                        Nothing ->
                            Err <| "No such file: " ++ path

                PackageLoader.ResolveDirectories dir ->
                    let
                        childPaths =
                            Dict.keys fileSystem
                                |> List.filter (childPackage dir)
                                |> List.map (String.replace "/stabel.json" "")
                                |> List.map PackagePath.Directory
                    in
                    resolveSideEffects
                        fileSystem
                        (sideEffect :: seenSfs)
                        (PackageLoader.update
                            (PackageLoader.ResolvedDirectories dir childPaths)
                            model
                        )

                PackageLoader.ResolvePackageModules packageName packagePath ->
                    let
                        srcPath =
                            packagePath ++ "/src/"

                        childPaths =
                            Dict.keys fileSystem
                                |> List.filter
                                    (\path ->
                                        String.startsWith srcPath path
                                    )
                                |> List.map (String.replace srcPath "")
                    in
                    resolveSideEffects
                        fileSystem
                        (sideEffect :: seenSfs)
                        (PackageLoader.update
                            (PackageLoader.ResolvedPackageModules packageName childPaths)
                            model
                        )


childPackage : String -> String -> Bool
childPackage targetDir path =
    if not <| String.startsWith targetDir path then
        False

    else
        let
            pathParts =
                path
                    |> String.replace (targetDir ++ "/") ""
                    |> String.split "/"
        in
        case pathParts of
            [ _, "stabel.json" ] ->
                True

            _ ->
                False


stdLibFiles : Dict String String
stdLibFiles =
    Dict.fromList
        [ ( "/project/lib/unused/stabel.json"
          , """
            {
                "name": "some/useless",
                "version": "1.7.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "useless/mod"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
          """
          )
        , ( "/project/lib/unused/src/useless/mod.stbl"
          , """
            def: square
            : dup *
            """
          )
        ]


testFiles : Dict String String
testFiles =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/fnv",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "mod1"
                ],
                "dependencies": {
                    "jarvis/template_strings": "1.2.0",
                    "stabel/version": "1.0.0"
                },
                "package-paths": [
                    "lib/template_strings",
                    "lib/version"
                ]
            }
            """
          )
        , ( "/project/src/mod1.stbl"
          , """
            def: next-version
            : /version/data/number 1 +
            """
          )
        , ( "/project/lib/template_strings/stabel.json"
          , """
            {
                "name": "jarvis/template_strings",
                "version": "1.2.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "template_strings/mod"
                ],
                "dependencies": {
                    "stabel/version": "1.1.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
          """
          )
        , ( "/project/lib/template_strings/src/template_strings/mod.stbl"
          , """
            def: dec
            : 1 =
            """
          )
        , ( "/project/lib/version/stabel.json"
          , """
            {
                "name": "stabel/version",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "version/data"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
          """
          )
        , ( "/project/lib/version/src/version/data.stbl"
          , """
            def: number
            : 1
            """
          )
        , ( "/project/lib/template_strings/lib/version/stabel.json"
          , """
            {
                "name": "stabel/version",
                "version": "1.2.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "version/data"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
          """
          )
        , ( "/project/lib/template_strings/lib/version/src/version/data.stbl"
          , """
            def: number
            : 2
            """
          )
        ]
        |> Dict.union stdLibFiles


testFilesInternalConsistency : Dict String String
testFilesInternalConsistency =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/dummy",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "mod1",
                    "mod2",
                    "mod3"
                ],
                "dependencies": {},
                "package-paths": []
            }
            """
          )
        , ( "/project/src/mod1.stbl"
          , """
            def: bump-version
            : 1 +
            """
          )
        , ( "/project/src/mod2.stbl"
          , """
            def: version
            : 5
            """
          )
        , ( "/project/src/mod3.stbl"
          , """
            def: next-version
            : mod2/version mod1/bump-version
            """
          )
        ]
        |> Dict.union stdLibFiles


testFilesAccessUnexposedModule : Dict String String
testFilesAccessUnexposedModule =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/dummy",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "core"
                ],
                "dependencies": {
                    "robheghan/pack": "1.0.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/src/core.stbl"
          , """
            def: test
            : /helper/number-six 4 +
            """
          )
        , ( "/project/lib/pack/stabel.json"
          , """
            {
                "name": "robheghan/pack",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "pub"
                ],
                "dependencies": {},
                "package-paths": []
            }
            """
          )
        , ( "/project/lib/pack/src/pub.stbl"
          , """
            def: number-five
            : 5
            """
          )
        , ( "/project/lib/pack/src/helper.stbl"
          , """
            def: number-six
            : 6
            """
          )
        ]
        |> Dict.union stdLibFiles


testFilesTransitiveDepBleed : Dict String String
testFilesTransitiveDepBleed =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/dummy",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "core"
                ],
                "dependencies": {
                    "robheghan/pack": "1.0.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/src/core.stbl"
          , """
            def: test
            : /helper/number-six
            """
          )
        , ( "/project/lib/pack/stabel.json"
          , """
            {
                "name": "robheghan/pack",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "pub"
                ],
                "dependencies": {
                    "robheghan/utils": "1.0.1"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/lib/pack/src/pub.stbl"
          , """
            def: number-five
            : /helper/number-six 1 -
            """
          )
        , ( "/project/lib/pack/lib/utils/stabel.json"
          , """
            {
                "name": "robheghan/utils",
                "version": "1.0.1",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "helper"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
            """
          )
        , ( "/project/lib/pack/lib/utils/src/helper.stbl"
          , """
            def: number-six
            : 6
            """
          )
        ]
        |> Dict.union stdLibFiles


testFilesTransitiveDep : Dict String String
testFilesTransitiveDep =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/dummy",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "core"
                ],
                "dependencies": {
                    "robheghan/pack": "1.0.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/src/core.stbl"
          , """
            def: test
            : /pub/number-five
            """
          )
        , ( "/project/lib/pack/stabel.json"
          , """
            {
                "name": "robheghan/pack",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "pub"
                ],
                "dependencies": {
                    "robheghan/utils": "1.0.1"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/lib/pack/src/pub.stbl"
          , """
            def: number-five
            : /helper/number-six 1 -
            """
          )
        , ( "/project/lib/pack/lib/utils/stabel.json"
          , """
            {
                "name": "robheghan/utils",
                "version": "1.0.1",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "helper"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
            """
          )
        , ( "/project/lib/pack/lib/utils/src/helper.stbl"
          , """
            def: number-six
            : 6
            """
          )
        ]
        |> Dict.union stdLibFiles


testFilesExternalRef : Dict String String
testFilesExternalRef =
    Dict.fromList
        [ ( "/project/stabel.json"
          , """
            {
                "name": "robheghan/dummy",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "core"
                ],
                "dependencies": {
                    "robheghan/pack": "1.0.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/src/core.stbl"
          , """
            def: test
            : /core/number-five 4 +
            """
          )
        , ( "/project/lib/pack/stabel.json"
          , """
            {
                "name": "robheghan/pack",
                "version": "1.0.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "core"
                ],
                "dependencies": {},
                "package-paths": []
            }
            """
          )
        , ( "/project/lib/pack/src/core.stbl"
          , """
            def: number-five
            : 5
            """
          )
        ]
        |> Dict.union stdLibFiles
