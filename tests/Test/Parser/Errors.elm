module Test.Parser.Errors exposing (..)

import Expect exposing (Expectation)
import Stabel.Parser exposing (..)
import Stabel.Parser.Problem exposing (Problem(..))
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (compile)


suite : Test
suite =
    describe "Parser -- Errors"
        [ describe "Double definitions" <|
            let
                wordAlreadyDefined name problem =
                    case problem of
                        WordAlreadyDefined definedName _ _ ->
                            name == definedName

                        _ ->
                            False
            in
            [ test "Word definition" <|
                \_ ->
                    checkForError (wordAlreadyDefined "not") <|
                        """
                        def: not
                        : drop 0

                        defmulti: not
                        : Int ( value 0)
                          drop 1
                        else: drop 0
                        """
            , test "Generated double definitions" <|
                \_ ->
                    checkForError (wordAlreadyDefined "age>") <|
                        """
                        def: age>
                        : 1

                        defstruct: Person
                        : age Int
                        """
            , test "Type definition" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: Person
                            : age Int

                            defunion: Person
                            : Person
                            """

                        typeAlreadyDefined name problem =
                            case problem of
                                TypeAlreadyDefined definedName _ _ ->
                                    name == definedName

                                _ ->
                                    False
                    in
                    checkForError (typeAlreadyDefined "Person") source
            ]
        , describe "Unknown metadata" <|
            let
                expectedError name problem =
                    case problem of
                        UnknownMetadata metaName ->
                            metaName == name

                        _ ->
                            False
            in
            [ test "word" <|
                \_ ->
                    checkForError (expectedError "typ") <|
                        """
                        def: inc
                        # typo
                        typ: Int -- Int
                        : 1 +
                        """
            , test "multiword" <|
                \_ ->
                    checkForError (expectedError "whn") <|
                        """
                        defmulti: double
                        # typo
                        whn: Int( value 0)
                          0
                        : 2 *
                        """
            , test "type" <|
                \_ ->
                    checkForError (expectedError "age") <|
                        """
                        defstruct: Person
                        # wrong syntax
                        age: Int
                        """
            , test "union" <|
                \_ ->
                    checkForError (expectedError "member") <|
                        """
                        defunion: Gender
                        member: Male
                        member: Female
                        """
            ]
        , test "Only compiler can create functions which names begins with an upper case letter" <|
            \_ ->
                checkForError ((==) NotSymbol) <|
                    """
                    def: Person
                    : 1
                    """
        , describe "Module path elements" <|
            let
                invalidModulePathError err =
                    case err of
                        InvalidModulePath _ ->
                            True

                        _ ->
                            False
            in
            [ test "Starting with upper case" <|
                \_ ->
                    checkForError invalidModulePathError <|
                        """
                        def: sample
                        : some/Other/module
                        """
            , test "Contains upper case" <|
                \_ ->
                    checkForError invalidModulePathError <|
                        """
                        def: sample
                        : some/otHer/module
                        """
            , test "Starting with upper case (external)" <|
                \_ ->
                    checkForError invalidModulePathError <|
                        """
                        def: sample
                        : /some/Other/module
                        """
            , test "Contains upper case (external)" <|
                \_ ->
                    checkForError invalidModulePathError <|
                        """
                        def: sample
                        : /some/otHer/module
                        """
            , test "External reference must contain two parts" <|
                \_ ->
                    checkForError invalidModulePathError <|
                        """
                        def: sample
                        : /some
                        """
            ]
        , describe "ModuleIsEmpty" <|
            let
                emptyModuleErr problem =
                    case problem of
                        ModuleIsEmpty ->
                            True

                        _ ->
                            False
            in
            [ test "Truly empty module" <|
                \_ ->
                    let
                        source =
                            """
                            """
                    in
                    checkForError emptyModuleErr source
            , test "Only module definition" <|
                \_ ->
                    let
                        source =
                            """
                            defmodule:
                            exposing: fn1
                            :
                            """
                    in
                    checkForError emptyModuleErr source
            ]
        ]


checkForError : (Problem -> Bool) -> String -> Expectation
checkForError fn source =
    case compile source of
        Err errors ->
            if List.any fn errors then
                Expect.pass

            else
                Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

        Ok _ ->
            Expect.fail "Did not expect parsing to succeed"
