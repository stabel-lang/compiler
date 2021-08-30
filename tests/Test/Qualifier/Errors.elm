module Test.Qualifier.Errors exposing (suite)

import Dict
import Expect exposing (Expectation)
import Set
import Stabel.Parser as Parser
import Stabel.Qualifier as Qualifier
import Stabel.Qualifier.Problem exposing (Problem(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier -- Errors"
        [ describe "No such reference" <|
            [ test "Word" <|
                \_ ->
                    let
                        source =
                            """
                            def: inc
                            : 1 +

                            def: main
                            : 1 inc inc dec 2 =
                            """
                    in
                    checkForError (noSuchWordReferenceError "dec") source
            , test "External" <|
                \_ ->
                    let
                        source =
                            """
                            def: main
                            : 1 /external/module/inc
                            """
                    in
                    checkForError (noSuchWordReferenceError "/external/module/inc") source
            , test "Type" <|
                \_ ->
                    let
                        source =
                            """
                            def: inc
                            type: Ints -- Int
                            : 1 +

                            def: main
                            : 1 inc 2 =
                            """
                    in
                    checkForError (noSuchTypeReferenceError "Ints") source
            , test "Wrong reference within union definition" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: USMoney
                            : Dollar
                            : Cent

                            defstruct: Dollar
                            : dollar-value Int
                            """
                    in
                    checkForError (noSuchTypeReferenceError "Cent") source
            , test "Wrong reference within custom type definition" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: BoxWrapper
                            : box Box
                            """
                    in
                    checkForError (noSuchTypeReferenceError "Box") source
            ]
        , test "Wrong reference within array literal" <|
            \_ ->
                let
                    source =
                        """
                        def: main
                        : { one two }

                        def: one
                        : 1
                        """
                in
                checkForError (noSuchWordReferenceError "two") source
        , test "Bad Int type" <|
            \_ ->
                let
                    source =
                        """
                        def: main
                        type: -- (Int a)
                        : 1
                        """

                    badIntError problem =
                        case problem of
                            BadIntType _ 1 ->
                                True

                            _ ->
                                False
                in
                checkForError badIntError source
        , test "Bad Array type" <|
            \_ ->
                let
                    source =
                        """
                        def: main
                        type: -- (Array a b)
                        : { 1 { 2 } }
                        """

                    badArrayError problem =
                        case problem of
                            BadArrayType _ 2 ->
                                True

                            _ ->
                                False
                in
                checkForError badArrayError source
        , test "Bad Array type (no arguments)" <|
            \_ ->
                let
                    source =
                        """
                        def: main
                        type: -- Array
                        : { 1 2 }
                        """

                    badArrayError problem =
                        case problem of
                            BadArrayType _ 0 ->
                                True

                            _ ->
                                False
                in
                checkForError badArrayError source
        ]


noSuchWordReferenceError : String -> Problem -> Bool
noSuchWordReferenceError name problem =
    case problem of
        UnknownFunctionRef _ problemName ->
            name == problemName

        _ ->
            False


noSuchTypeReferenceError : String -> Problem -> Bool
noSuchTypeReferenceError name problem =
    case problem of
        UnknownTypeRef _ problemName ->
            name == problemName

        _ ->
            False


checkForError : (Problem -> Bool) -> String -> Expectation
checkForError fn source =
    case Parser.run "test" source of
        Err errors ->
            Expect.fail <| "Parser error: " ++ Debug.toString errors

        Ok parserAst ->
            let
                result =
                    Qualifier.run
                        { packageName = ""
                        , modulePath = ""
                        , ast = parserAst
                        , externalModules = Dict.empty
                        , inProgressAST =
                            { types = Dict.empty
                            , functions = Dict.empty
                            , referenceableFunctions = Set.empty
                            }
                        }
            in
            case result of
                Err errors ->
                    if List.any fn errors then
                        Expect.pass

                    else
                        Expect.fail <| "Failed for unexpected reason: " ++ Debug.toString errors

                Ok _ ->
                    Expect.fail "Did not expect parsing to succeed"
