module Test.TypeChecker.Unions exposing (suite)

import Dict.Extra as Dict
import Stabel.Data.Builtin as Builtin
import Stabel.Data.SourceLocation exposing (emptyRange)
import Stabel.Data.Type as Type
import Stabel.Qualifier as QAST
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
    describe "TypeChecker -- Unions and multifunctions"
        [ test "Simplest case" <|
            \_ ->
                let
                    input =
                        template ++ """
                        defmulti: to-int
                        : False
                          drop 0
                        : True
                          drop 1
                        """
                in
                expectTypeCheck input
        , test "With type signature" <|
            \_ ->
                let
                    input =
                        template ++ """
                        defmulti: to-int
                        type: Bool -- Int
                        : False
                          drop 0
                        : True
                          drop 1
                        """
                in
                expectTypeCheck input
        , test "With default branch" <|
            \_ ->
                let
                    input =
                        template ++ """
                        defmulti: to-int
                        type: Bool -- Int
                        : False
                          drop 0
                        else:
                          drop 1
                        """
                in
                expectTypeCheck input
        , test "With default branch (no type meta)" <|
            \_ ->
                let
                    input =
                        template ++ """
                        defmulti: to-int
                        : False
                          drop 0
                        else:
                          drop 1
                        """
                in
                expectTypeCheck input
        , test "When returning union" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Beings
                        : Person
                        : Dog

                        defstruct: Person
                        : age Int

                        defstruct: Dog
                        : man-years Int

                        defmulti: add-to-age
                        : Person
                          >age
                        : Dog
                          4 * >man-years

                        defmulti: get-man-age
                        : Person
                          age>
                        : Dog
                          man-years>

                        def: main
                        : 18 >Person
                          10 add-to-age

                          0 >Dog
                          2 add-to-age

                          get-man-age
                          swap get-man-age
                          -
                        """
                in
                expectTypeCheck input
        , test "Function requiring a concrete type should not accept an union with that type" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Bool
                        : True
                        : False

                        defstruct: True
                        defstruct: False

                        defmulti: not
                        : True
                          drop False>
                        : False
                          drop True>

                        def: true-to-int
                        type: True -- Int
                        : drop 1

                        def: main
                        : True> not true-to-int
                        """
                in
                expectTypeCheckFailure input
        , test "Generic union" <|
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

                        defmulti: first-or-default
                        type: (List a) a -- a
                        : NonEmptyList
                          drop first>
                        : EmptyList
                          swap drop

                        def: main
                        : 1 EmptyList> >NonEmptyList
                          0 first-or-default
                          1 =
                        """
                in
                expectTypeCheck input
        , test "Union with generic branch" <|
            \_ ->
                let
                    input =
                        """
                        defunion: Maybe a
                        : a
                        : Nil

                        defstruct: Nil

                        defmulti: with-default
                        type: (Maybe a) a -- a
                        : a
                          drop
                        : Nil
                          swap drop

                        def: main
                        : Nil> 1 with-default
                        """

                    nilTypeDef =
                        { name = "Nil"
                        , exposed = True
                        , generics = []
                        , sourceLocation = emptyRange
                        , members = QAST.StructMembers []
                        }

                    nilCtorDef =
                        { name = "Nil>"
                        , type_ = { input = [], output = [ Type.Custom "Nil" ] }
                        , sourceLocation = Nothing
                        , implementation =
                            SoloImpl
                                [ ConstructType nilTypeDef ]
                        }

                    withDefaultFn =
                        { name = "with-default"
                        , type_ =
                            { input =
                                [ Type.Union (Just "Maybe")
                                    [ Type.Generic "a"
                                    , Type.Custom "Nil"
                                    ]
                                , Type.Generic "a"
                                ]
                            , output = [ Type.Generic "a" ]
                            }
                        , sourceLocation = Nothing
                        , implementation =
                            MultiImpl
                                [ ( TypeMatchType emptyRange (Type.Generic "a") []
                                  , [ Builtin emptyRange Builtin.StackDrop
                                    ]
                                  )
                                , ( TypeMatchType emptyRange (Type.Custom "Nil") []
                                  , [ Builtin emptyRange Builtin.StackSwap
                                    , Builtin emptyRange Builtin.StackDrop
                                    ]
                                  )
                                ]
                                []
                        }

                    expectedResult =
                        Dict.fromListBy .name
                            [ nilCtorDef
                            , { name = "main"
                              , type_ = { input = [], output = [ Type.Int ] }
                              , sourceLocation = Nothing
                              , implementation =
                                    SoloImpl
                                        [ Function emptyRange
                                            nilCtorDef
                                            { input = []
                                            , output = [ Type.Custom "Nil" ]
                                            }
                                        , IntLiteral emptyRange 1
                                        , Function emptyRange
                                            withDefaultFn
                                            { input =
                                                [ Type.Union (Just "Maybe")
                                                    [ Type.Int
                                                    , Type.Custom "Nil"
                                                    ]
                                                , Type.Int
                                                ]
                                            , output = [ Type.Int ]
                                            }
                                        ]
                              }
                            , withDefaultFn
                            ]
                in
                expectAst input expectedResult
        , test "Generic union fails if not generic is listed" <|
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
                expectTypeCheckFailure input
        ]


template : String
template =
    """
    defunion: Bool
    : True
    : False

    defstruct: True
    defstruct: False

    def: main
    : True> to-int
      False> to-int
      =
    """
