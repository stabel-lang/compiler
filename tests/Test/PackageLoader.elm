module Test.PackageLoader exposing (suite)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Expect exposing (Expectation)
import List.Extra as List
import Play.Data.Builtin as Builtin
import Play.Data.Metadata as Metadata
import Play.Data.PackagePath as PackagePath
import Play.Data.SourceLocation exposing (emptyRange)
import Play.PackageLoader as PackageLoader
import Play.Qualifier as Qualifier
import Test exposing (Test, describe, test)
import Test.Qualifier.Util as Util


suite : Test
suite =
    describe "PackageLoader"
        [ test "Passes the load package metadata step" <|
            \_ ->
                PackageLoader.init "/project"
                    |> Expect.equal (PackageLoader.Initializing <| PackageLoader.ReadFile "/project" "play.json")
        , test "Retrieves necessary files" <|
            \_ ->
                PackageLoader.init "/project"
                    |> expectSideEffects testFiles
                        [ PackageLoader.ReadFile "/project" "play.json"
                        , PackageLoader.ResolveDirectories "/project/lib"
                        , PackageLoader.ReadFile "/project/lib/template_strings" "play.json"
                        , PackageLoader.ReadFile "/project/lib/template_strings/lib/version" "play.json"
                        , PackageLoader.ReadFile "/project/lib/unused" "play.json"
                        , PackageLoader.ReadFile "/project/lib/version" "play.json"
                        , PackageLoader.ResolvePackageModules "robheghan/fnv" "/project"
                        , PackageLoader.ResolvePackageModules "jarvis/template_strings" "/project/lib/template_strings"
                        , PackageLoader.ResolvePackageModules "play/version" "/project/lib/template_strings/lib/version"
                        , PackageLoader.ReadFile "/project/src" "mod1.play"
                        , PackageLoader.ReadFile "/project/lib/template_strings/lib/version/src/version" "data.play"
                        ]
        , test "Compiles to qualified AST" <|
            \_ ->
                let
                    loaderResult =
                        PackageLoader.init "/project"
                            |> resolveSideEffects testFiles []
                            |> Result.map Tuple.second
                in
                case loaderResult of
                    Err msg ->
                        Expect.fail msg

                    Ok ast ->
                        Expect.equal
                            { types =
                                Dict.fromListBy Qualifier.typeDefinitionName []
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "/robheghan/fnv/mod1/next-version"
                                      , metadata = Metadata.default
                                      , implementation =
                                            Qualifier.SoloImpl
                                                [ Qualifier.Word emptyRange "/play/version/version/data/number"
                                                , Qualifier.Integer emptyRange 1
                                                , Qualifier.Builtin emptyRange Builtin.Plus
                                                ]
                                      }
                                    , { name = "/play/version/version/data/number"
                                      , metadata = Metadata.default
                                      , implementation =
                                            Qualifier.SoloImpl
                                                [ Qualifier.Integer emptyRange 2
                                                ]
                                      }
                                    ]
                            }
                            (Util.stripLocations ast)
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
            in
            Expect.equalLists [] sfDiff


resolveSideEffects :
    Dict String String
    -> List PackageLoader.SideEffect
    -> PackageLoader.Model
    -> Result String ( List PackageLoader.SideEffect, Qualifier.ExposedAST )
resolveSideEffects fileSystem seenSfs model =
    case getSideEffect model of
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
                                |> List.map (String.replace "/play.json" "")
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
            [ _, "play.json" ] ->
                True

            _ ->
                False


getSideEffect : PackageLoader.Model -> Maybe PackageLoader.SideEffect
getSideEffect model =
    case model of
        PackageLoader.Done _ ->
            Nothing

        PackageLoader.Failed _ ->
            Nothing

        PackageLoader.Initializing sf ->
            Just sf

        PackageLoader.LoadingMetadata _ _ sf ->
            Just sf

        PackageLoader.ResolvingModulePaths _ _ sf ->
            Just sf

        PackageLoader.Compiling _ _ sf ->
            Just sf


testFiles : Dict String String
testFiles =
    Dict.fromList
        [ ( "/project/play.json"
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
                    "play/version": "1.0.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/src/mod1.play"
          , """
            def: next-version
            : /version/data/number 1 +
            """
          )
        , ( "/project/lib/template_strings/play.json"
          , """
            {
                "name": "jarvis/template_strings",
                "version": "1.2.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "template_strings/mod"
                ],
                "dependencies": {
                    "play/version": "1.1.0"
                },
                "package-paths": [
                    "lib/version"
                ]
            }
          """
          )
        , ( "/project/lib/template_strings/src/template_strings/mod.play"
          , """
            def: dec
            : 1 =
            """
          )
        , ( "/project/lib/unused/play.json"
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
        , ( "/project/lib/unused/src/useless/mod.play"
          , """
            def: square
            : dup *
            """
          )
        , ( "/project/lib/version/play.json"
          , """
            {
                "name": "play/version",
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
        , ( "/project/lib/version/src/version/data.play"
          , """
            def: number
            : 1
            """
          )
        , ( "/project/lib/template_strings/lib/version/play.json"
          , """
            {
                "name": "play/version",
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
        , ( "/project/lib/template_strings/lib/version/src/version/data.play"
          , """
            def: number
            : 2
            """
          )
        ]
