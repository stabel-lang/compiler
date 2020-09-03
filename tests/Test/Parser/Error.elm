module Test.Parser.Error exposing (..)

import Expect
import Play.Parser exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (compile)


suite : Test
suite =
    describe "Parser errors"
        [ describe "double definitions"
            [ test "Word definition" <|
                \_ ->
                    let
                        source =
                            """
                            def: not
                            : drop 0

                            defmulti: not
                            when: Int ( value 0)
                              drop 1
                            : drop 0
                            """

                        wordAlreadyDefined name deadend =
                            case deadend.problem of
                                WordAlreadyDefined definedName _ _ ->
                                    name == definedName

                                _ ->
                                    False
                    in
                    case compile source of
                        Err errors ->
                            if List.any (wordAlreadyDefined "not") errors then
                                Expect.pass

                            else
                                Expect.fail "Failed for unexpected reason"

                        Ok _ ->
                            Expect.fail "Did not expect parsing to succeed"
            , test "Generated double definitions" <|
                \_ ->
                    let
                        source =
                            """
                            def: age>
                            : 1

                            deftype: Person
                            : age Int
                            """

                        wordAlreadyDefined name deadend =
                            case deadend.problem of
                                WordAlreadyDefined definedName _ _ ->
                                    name == definedName

                                _ ->
                                    False
                    in
                    case compile source of
                        Err errors ->
                            if List.any (wordAlreadyDefined "age>") errors then
                                Expect.pass

                            else
                                Expect.fail "Failed for unexpected reason"

                        Ok _ ->
                            Expect.fail "Did not expect parsing to succeed"
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

                        wordAlreadyDefined name deadend =
                            case deadend.problem of
                                TypeAlreadyDefined definedName _ _ ->
                                    name == definedName

                                _ ->
                                    False
                    in
                    case compile source of
                        Err errors ->
                            if List.any (wordAlreadyDefined "Person") errors then
                                Expect.pass

                            else
                                Expect.fail "Failed for unexpected reason"

                        Ok _ ->
                            Expect.fail "Did not expect parsing to succeed"
            ]
        ]
