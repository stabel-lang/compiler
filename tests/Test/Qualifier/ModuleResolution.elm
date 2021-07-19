module Test.Qualifier.ModuleResolution exposing (suite)

import Expect exposing (Expectation)
import Stabel.Qualifier exposing (..)
import Stabel.Qualifier.Problem as Problem exposing (Problem)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier -- Module resolution"
        [ test "Referencing a function from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/test/internal/mod"
                          , """
                            defmodule:
                            exposes: dummy
                            :

                            def: value
                            : 1

                            def: dummy
                            : 1
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defmodule:
                            import: internal/mod
                            :

                            def: main
                            : value
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.FunctionNotExposed _ "internal/mod/value" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a function from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/external/mod"
                          , """
                            defmodule:
                            exposes: dummy
                            :

                            def: add
                            : 1

                            def: dummy
                            : 1
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defmodule:
                            import: /mod
                            :

                            def: main
                            : 1 2 add
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.FunctionNotExposed _ "/stabel/external/mod/add" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type signature from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/external/mod"
                          , """
                            defmodule:
                            exposes: dummy
                            :

                            defstruct: Tipe

                            def: dummy
                            : 1
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            def: main
                            type: /mod/Tipe
                            : drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type definition from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/external/mod"
                          , """
                            defmodule:
                            exposes: Tipe
                            :

                            defunion: TipeUnion
                            : a
                            : Tipe

                            defstruct: Tipe
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defstruct: BoxedTipe
                            : value /mod/TipeUnion
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/TipeUnion" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type match from an external module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/external/mod"
                          , """
                            defmodule:
                            exposes: Dummy
                            :

                            defstruct: Tipe
                            defstruct: Dummy
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defmulti: call
                            : /mod/Tipe
                              drop
                            else: drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/external/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type signature from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/test/mod"
                          , """
                            defmodule:
                            exposes: Dummy
                            :

                            defstruct: Tipe
                            defstruct: Dummy
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            def: call
                            type: mod/Tipe --
                            : drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type definition from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/test/mod"
                          , """
                            defmodule:
                            exposes: Tipe
                            :

                            defunion: TipeUnion
                            : Tipe
                            : a

                            defstruct: Tipe
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defstruct: BoxedTipe
                            : value mod/TipeUnion
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/TipeUnion" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        , test "Referencing a type in a type match from an internal module which isn't exposed ends in a error" <|
            \_ ->
                let
                    sources =
                        [ ( "/stabel/test/mod"
                          , """
                            defmodule:
                            exposes: TipeUnion
                            :

                            defunion: TipeUnion
                            : Tipe
                            : a

                            defstruct: Tipe
                            """
                          )
                        , ( "/stabel/test/core"
                          , """
                            defmulti: call
                            : mod/Tipe
                              drop
                            else: drop
                            """
                          )
                        ]

                    findError err =
                        case err of
                            Problem.TypeNotExposed _ "/stabel/test/mod/Tipe" ->
                                True

                            _ ->
                                False
                in
                checkForError findError sources
        ]


checkForError : (Problem -> Bool) -> List ( String, String ) -> Expectation
checkForError pred sources =
    Expect.fail ""
