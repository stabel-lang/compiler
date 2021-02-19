module Test.PackageLoader exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import List.Extra as List
import Play.Data.PackagePath as PackagePath
import Play.PackageLoader as PackageLoader
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PackageLoader"
        [ test "Passes the load package metadata step" <|
            \_ ->
                PackageLoader.init "/project"
                    |> Tuple.second
                    |> Expect.equal (PackageLoader.ReadFile "/project" "play.json")
        , test "Retrieves necessary files" <|
            \_ ->
                PackageLoader.init "/project"
                    |> expectSideEffects testFiles
                        [ PackageLoader.ReadFile "/project" "play.json"
                        , PackageLoader.ResolveDirectories "/project/lib"
                        , PackageLoader.ReadFile "/project/lib/template_strings" "play.json"
                        ]
        ]


expectSideEffects : Dict String String -> List PackageLoader.SideEffect -> ( PackageLoader.Model, PackageLoader.SideEffect ) -> Expectation
expectSideEffects fileSystem expectedSFs ( model, sideEffect ) =
    case PackageLoader.doneState model of
        Just _ ->
            Expect.equalLists [] expectedSFs

        Nothing ->
            case sideEffect of
                PackageLoader.ReadFile path filename ->
                    case Dict.get (path ++ "/" ++ filename) fileSystem of
                        Just fileContent ->
                            expectSideEffects
                                fileSystem
                                (List.remove sideEffect expectedSFs)
                                (PackageLoader.update
                                    (PackageLoader.FileContents path filename fileContent)
                                    model
                                )

                        Nothing ->
                            Expect.fail <| "No such file: " ++ path

                PackageLoader.ResolveDirectories dir ->
                    let
                        childPaths =
                            Dict.keys fileSystem
                                |> List.filter
                                    (\path ->
                                        String.startsWith dir path
                                            && String.endsWith "play.json" path
                                    )
                                |> List.map (String.replace "/play.json" "")
                                |> List.map PackagePath.Directory
                    in
                    expectSideEffects
                        fileSystem
                        (List.remove sideEffect expectedSFs)
                        (PackageLoader.update
                            (PackageLoader.ResolvedDirectories dir childPaths)
                            model
                        )

                PackageLoader.NoOp ->
                    Expect.fail "Did not expect NoOp message."


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
                    "fnv"
                ],
                "dependencies": {
                    "jarvis/template_strings": "1.2.0"
                },
                "package-paths": [
                    "lib/*"
                ]
            }
            """
          )
        , ( "/project/lib/template_strings/play.json"
          , """
            {
                "name": "jarvis/template_strings",
                "version": "1.2.0",
                "language-version": "0.2.0",
                "exposed-modules": [
                    "template_strings"
                ],
                "dependencies": {
                },
                "package-paths": [
                ]
            }
          """
          )
        ]
