module Test.Parser.Error exposing (..)

import Expect exposing (Expectation)
import Play.Parser exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (compile)


suite : Test
suite =
    describe "Parser errors"
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
                        when: Int ( value 0)
                          drop 1
                        : drop 0
                        """
            , test "Generated double definitions" <|
                \_ ->
                    checkForError (wordAlreadyDefined "age>") <|
                        """
                        def: age>
                        : 1

                        deftype: Person
                        : age Int
                        """
            , test "Type definition" <|
                \_ ->
                    let
                        source =
                            """
                            deftype: Person
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
                        deftype: Person
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
