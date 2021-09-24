module Test.TypeChecker.Errors exposing (suite)

import Expect
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.TypeChecker.Problem as Problem exposing (Problem)
import Test exposing (Test, describe, test)
import Test.TypeChecker.Util as Util exposing (checkForError)


suite : Test
suite =
    describe "TypeChecker -- Errors"
        [ test "Undeclared generic in generic struct" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box a
                        : value b
                        """
                in
                checkForError (undeclaredGenericError "b") input
        , test "Undeclared generic in struct" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box
                        : value a
                        """
                in
                checkForError (undeclaredGenericError "a") input
        , test "Undeclared generic union" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Maybe
                        : a
                        : Nothing

                        defstruct: Nothing
                        """
                in
                checkForError (undeclaredGenericError "a") input
        , test "Undeclared generic in generic union" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Maybe a
                        : b
                        : Nothing

                        defstruct: Nothing
                        """
                in
                checkForError (undeclaredGenericError "b") input
        , test "Wrong type signature" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: -- Int
                        : 1 2
                        """

                    unexpectedType problem =
                        case problem of
                            Problem.TypeError _ "main" provided inferred ->
                                (provided == { input = [], output = [ Type.Int ] })
                                    && (inferred == { input = [], output = [ Type.Int, Type.Int ] })

                            _ ->
                                False
                in
                checkForError unexpectedType input
        , test "Unexpected function" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: IntBox
                        : value Int

                        def: main
                        type: -- Int
                        : 1 value>
                        """

                    unexpectedType problem =
                        case problem of
                            Problem.UnexpectedType _ "main" (Type.Custom "IntBox") Type.Int ->
                                True

                            _ ->
                                False
                in
                checkForError unexpectedType input
        , test "An inferred concrete output type should not successfully type check against a generic variable" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: in -- out
                        : 1 +
                        """

                    typeError problem =
                        case problem of
                            Problem.TypeError _ "main" _ _ ->
                                True

                            _ ->
                                False
                in
                checkForError typeError input
        , test "An inferred concrete input type should not successfully type check against a generic variable" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: in -- Int
                        : 1 +
                        """

                    typeError problem =
                        case problem of
                            Problem.TypeError _ "main" _ _ ->
                                True

                            _ ->
                                False
                in
                checkForError typeError input
        , test "An inferred concrete struct input type should not successfully type check against a generic struct" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box a
                        : value a

                        def: box-inc
                        type: (Box a) -- Int
                        : value> 1 +

                        def: main
                        : 1 >Box box-inc
                        """

                    typeError problem =
                        case problem of
                            Problem.TypeError _ "box-inc" _ _ ->
                                True

                            _ ->
                                False
                in
                checkForError typeError input
        , test "An inferred concrete union input type should not successfully type check against a generic union" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Maybe a
                        : a
                        : Nothing

                        defstruct: Nothing

                        defmulti: maybe-inc
                        type: (Maybe a) -- Int
                        : a
                          1 +
                        : Nothing
                          drop 0

                        def: main
                        : 1 maybe-inc
                        """

                    typeError problem =
                        case problem of
                            Problem.InconsistentWhens _ "maybe-inc" ->
                                True

                            _ ->
                                False
                in
                checkForError typeError input
        , test "An inferred union output type should not successfully type check against a generic variable" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Bool
                        : True
                        : False

                        defstruct: True
                        defstruct: False

                        defunion: Tmp a b
                        : a
                        : b

                        def: main
                        type: -- out
                        : 0 true-or-false

                        defmulti: true-or-false
                        type: Int -- (Tmp a b)
                        : 0
                          drop False>
                        : Int
                          drop True>
                        """
                in
                Expect.equalLists
                    [ Problem.TypeError emptyRange
                        "main"
                        { input = [], output = [ Type.Generic "a" ] }
                        { input = []
                        , output =
                            [ Type.Union (Just "Tmp")
                                [ Type.Generic "b", Type.Generic "a" ]
                            ]
                        }
                    , Problem.TypeError emptyRange
                        "true-or-false"
                        { input = [ Type.Int ]
                        , output =
                            [ Type.Union (Just "Tmp")
                                [ Type.Generic "b", Type.Generic "a" ]
                            ]
                        }
                        { input = [ Type.Int ]
                        , output =
                            [ Type.Union Nothing
                                [ Type.Custom "False", Type.Custom "True" ]
                            ]
                        }
                    ]
                    (Util.getTypeErrors input)
        , test "Type checker should detect and fail when the number of outputs doesn't match" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Bool
                        : True
                        : False

                        defstruct: True
                        defstruct: False

                        defmulti: true-or-false
                        # should be Int -- Int (Maybe a)
                        type: Int -- Int
                        : 0
                          False>
                        : Int
                          True>
                        """
                in
                Expect.equalLists
                    [ Problem.TypeError emptyRange
                        "true-or-false"
                        { input = [ Type.Int ]
                        , output = [ Type.Int ]
                        }
                        { input = [ Type.Int ]
                        , output =
                            [ Type.Int
                            , Type.Union Nothing
                                [ Type.Custom "False", Type.Custom "True" ]
                            ]
                        }
                    ]
                    (Util.getTypeErrors input)
        , describe "Inexhaustiveness checking"
            [ test "Simple example" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 2 mword

                            defmulti: mword
                            : 1
                              1 +
                            """

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiFunction _ [ [ Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError input
            , test "Default clause is exhaustive" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 2 mword

                            defmulti: mword
                            : 1
                              1 +
                            else: 
                              0 +
                            """
                    in
                    Util.expectTypeCheck input
            , test "Nested" <|
                \_ ->
                    let
                        input =
                            """
                            defstruct: IntBox
                            : value Int

                            def: main
                            type: -- Int
                            : 1 >IntBox mword value>

                            defmulti: mword
                            : IntBox( value 1 )
                              value> 1 + >IntBox
                            """

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiFunction _ [ [ Type.Custom "IntBox", Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError input
            , test "Default clause is exhaustive in case of nested match" <|
                \_ ->
                    let
                        input =
                            """
                            defstruct: Box
                            : value Int

                            def: main
                            : 2 >Box mword

                            defmulti: mword
                            : Box( value 1 )
                              drop 1
                            else: 
                              drop 0
                            """
                    in
                    Util.expectTypeCheck input
            , test "A total branch should remove any earlier seen branch" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 2 mword

                            defmulti: mword
                            : 1
                              1 +
                            : Int
                              dup +
                            """
                    in
                    Util.expectTypeCheck input
            , test "A total branch should prevent addition of later partial branch" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 2 mword

                            defmulti: mword
                            : Int
                              dup +
                            : 1
                              1 +
                            """
                    in
                    Util.expectTypeCheck input
            , test "Test with non-int type as pattern" <|
                \_ ->
                    let
                        input =
                            """
                            defunion: Maybe a
                            : a
                            : Nil

                            defstruct: IntBox
                            : value Int

                            defstruct: Nil

                            defmulti: with-default
                            type: (Maybe IntBox) Int -- Int
                            : IntBox( value 0 )
                              drop value>
                            : Nil
                              swap drop

                            def: main
                            : Nil> 1 with-default
                            """

                        inexhaustiveError problem =
                            case problem of
                                Problem.InexhaustiveMultiFunction _ [ [ Type.Custom "IntBox", Type.Int ] ] ->
                                    True

                                _ ->
                                    False
                    in
                    checkForError inexhaustiveError input
            ]
        ]


undeclaredGenericError : String -> Problem -> Bool
undeclaredGenericError generic problem =
    case problem of
        Problem.UndeclaredGeneric _ problemGeneric ->
            generic == problemGeneric

        _ ->
            False
