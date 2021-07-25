module Test.TypeChecker exposing (suite)

import Dict
import Dict.Extra as Dict
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.TypeChecker exposing (..)
import Test exposing (Test, describe, test)
import Test.TypeChecker.Util
    exposing
        ( expectAst
        , expectTypeCheck
        , expectTypeCheckFailure
        )


suite : Test
suite =
    describe "TypeChecker"
        [ test "Bad type annotation" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: Int --
                        : 1 2 =
                        """
                in
                expectTypeCheckFailure input
        , test "Generic types" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: -- Int
                        : 1 2 over + - 2 =

                        def: over
                        type: b c -- b c b
                        : swap dup rotate
                        """
                in
                expectTypeCheck input
        , test "Generic types with type annotation" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        type: -- Int
                        : 5 square

                        def: square
                        type: Int -- Int
                        : dup *
                        """
                in
                expectTypeCheck input
        , test "Generic custom type" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box a
                        : element a

                        def: main
                        type: -- Int
                        : 5 >Box element>
                          10 +
                          15 =
                        """
                in
                expectTypeCheck input
        , test "Generic types with rotations and quotations" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Coordinate
                        : x Int
                        : y Int

                        def: main
                        type: -- Int
                        : 1 2 >Coordinate
                          [ 1 + ] update-x
                          x>

                        def: update-x
                        type: Coordinate [ Int -- Int ] -- Coordinate
                        : swap dup x> # [ Int -- Int] Coordinate x
                          -rotate !
                          >x
                        """
                in
                expectTypeCheck input
        , test "Generic types with generic quotations" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Pair a b
                        : first a
                        : second b

                        defunion: List a
                        : NonEmpty a
                        : Empty

                        defstruct: NonEmpty a
                        : head a
                        : rest (List a)

                        defstruct: Empty

                        def: main
                        type: -- Int
                        : 1 2 3 Empty >NonEmpty >NonEmpty >NonEmpty
                          0 [ + ] fold

                        defmulti: fold
                        type: (List a) b [ a b -- b ] -- b
                        : Empty
                          drop swap drop
                        : NonEmpty
                          >Pair swap
                          [ head> ] [ rest> ] split
                          rotate swap dup 
                          rotate
                          spill !
                          swap second>
                          fold

                        def: split
                        type: a [ a -- b ] [ a -- c ] -- b c
                        : -rotate dup -rotate
                          !
                          swap -rotate
                          !
                          swap

                        def: spill
                        type: (Pair a b) -- a b
                        : [ first> ] [ second> ] split
                        """
                in
                expectTypeCheck input
        , test "Generic custom type fails if not generic is listed" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box
                        : element a
                        """
                in
                expectTypeCheckFailure input
        , test "Generic custom type fails if wrong generic is listed" <|
            \_ ->
                let
                    input =
                        """
                        defstruct: Box a
                        : element b
                        """
                in
                expectTypeCheckFailure input
        , describe "Quotations"
            [ test "Simple example" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 1
                              [ 1 + ] apply-to-num
                              [ 1 - ] apply-to-num

                            def: apply-to-num
                            : !
                            """
                    in
                    expectTypeCheck input
            , test "With type annotation" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 1
                              [ 1 + ] apply-to-num
                              [ 1 - ] apply-to-num

                            def: apply-to-num
                            type: Int [ Int -- Int ] -- Int
                            : !
                            """
                    in
                    expectTypeCheck input
            , test "Typechecking involving a multi-arity quotation is fine _if_ arity info is in type annotation" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : [ + ] apply-to-nums

                            def: apply-to-nums
                            type: [ Int Int -- Int ] -- Int
                            : 1 2 -rotate !
                            """
                    in
                    expectTypeCheck input
            , test "With generics" <|
                \_ ->
                    let
                        input =
                            """
                            def: main
                            : 1 [ 1 - ] map

                            def: map
                            type: a [ a -- b ] -- b
                            : !
                            """
                    in
                    expectTypeCheck input
            , test "Within multiwords" <|
                \_ ->
                    let
                        input =
                            """
                            defunion: Maybe a
                            : a
                            : Nil

                            defstruct: Nil

                            defmulti: map
                            type: (Maybe a) [ a -- b ] -- (Maybe b)
                            : a
                              !
                            : Nil
                              drop

                            def: main
                            : Nil [ 1 - ] map
                            """
                    in
                    expectTypeCheck input
            ]
        , describe "Recursive word definitions"
            [ test "With type annotation" <|
                \_ ->
                    let
                        input =
                            """
                            defunion: List a
                            : NonEmptyList a
                            : EmptyList

                            defstruct: NonEmptyList a
                            : first a
                            : rest (List a)

                            defstruct: EmptyList

                            def: sum
                            : 0 sum-helper

                            defmulti: sum-helper
                            type: (List a) Int -- Int
                            : NonEmptyList
                              swap rest> swap 
                              1 +
                              sum-helper
                            : EmptyList
                              swap drop

                            def: main
                            : 1 2 3 EmptyList >NonEmptyList >NonEmptyList >NonEmptyList
                              sum
                            """
                    in
                    expectTypeCheck input
            ]
        , test "Correct node types" <|
            \_ ->
                let
                    input =
                        """
                        def: main
                        : 1 2 drop-first

                        def: drop-first
                        type: a b -- b
                        : swap drop
                        """

                    expectedResult =
                        Dict.fromListBy .name
                            [ { name = "main"
                              , type_ =
                                    { input = []
                                    , output = [ Type.Int ]
                                    }
                              , sourceLocation = Nothing
                              , isInline = False
                              , implementation =
                                    SoloImpl
                                        [ IntLiteral emptyRange 1
                                        , IntLiteral emptyRange 2
                                        , Function emptyRange
                                            "drop-first"
                                            { input = [ Type.Int, Type.Int ]
                                            , output = [ Type.Int ]
                                            }
                                        ]
                              }
                            , { name = "drop-first"
                              , type_ =
                                    { input = [ Type.Generic "a", Type.Generic "b" ]
                                    , output = [ Type.Generic "b" ]
                                    }
                              , sourceLocation = Nothing
                              , isInline = False
                              , implementation =
                                    SoloImpl
                                        [ Builtin emptyRange Builtin.StackSwap
                                        , Builtin emptyRange Builtin.StackDrop
                                        ]
                              }
                            ]
                in
                expectAst input expectedResult
        ]
