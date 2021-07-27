module Test.TypeChecker.Errors exposing (suite)

import Expect
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Qualifier exposing (..)
import Stabel.TypeChecker.Problem as Problem
import Test exposing (Test, describe, test)
import Test.TypeChecker.Util as Util exposing (checkForError)


suite : Test
suite =
    describe "TypeChecker -- Errors"
        [ test "Undeclared generic" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box a
                        : value b
                        """

                    undeclaredGenericError generic problem =
                        case problem of
                            Problem.UndeclaredGeneric _ problemGeneric _ ->
                                generic == problemGeneric

                            _ ->
                                False
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

                    undeclaredGenericError problem =
                        case problem of
                            Problem.TypeError _ "main" provided inferred ->
                                (provided == { input = [], output = [ Type.Int ] })
                                    && (inferred == { input = [], output = [ Type.Int, Type.Int ] })

                            _ ->
                                False
                in
                checkForError undeclaredGenericError input
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

                    undeclaredGenericError problem =
                        case problem of
                            Problem.UnexpectedType _ "main" (Type.Custom "IntBox") Type.Int ->
                                True

                            _ ->
                                False
                in
                checkForError undeclaredGenericError input
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
        , test "An inferred concrete output type should not successfully type check against a generic variable, multi-fn" <|
            \_ ->
                let
                    input =
                        """
                        defunion: List a
                        : NonEmptyList a 
                        : EmptyList

                        defstruct: NonEmptyList a
                        : first a
                        : rest List a

                        defstruct: EmptyList

                        def: sum
                        : 0 sum-helper

                        defmulti: sum-helper
                        type: (List a) Int -- Int
                        : NonEmptyList
                          swap dup first>
                          rotate rest> 
                          rotate + 
                          sum-helper
                        : EmptyList
                          swap drop

                        def: main
                        : 1 2 3 EmptyList >NonEmptyList >NonEmptyList >NonEmptyList
                        sum
                        """

                    typeError problem =
                        case problem of
                            Problem.TypeError _ "sum-helper" _ _ ->
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
                        : Int( value 0 )
                          drop False
                        : Int
                          drop True
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
                        : Int( value 0 )
                          False
                        : Int
                          True
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
                            : Int( value 1 )
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
                            : Int( value 1 )
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
                            : IntBox( value Int( value 1 ) )
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
            , test "A total branch should remove any earlier seen branch" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 2 mword

                            defmulti: mword
                            : Int( value 1 )
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
                            : Int( value 1 )
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
                            : IntBox( value Int( value 0 ) )
                              drop value>
                            : Nil
                              swap drop

                            def: main
                            : Nil 1 with-default
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
