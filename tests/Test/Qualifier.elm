module Test.Qualifier exposing (..)

import Dict exposing (Dict)
import Expect
import Play.Data.Metadata as Metadata
import Play.Parser as AST
import Play.Qualifier exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Qualifier"
        [ test "Simple program" <|
            \_ ->
                let
                    defaultMeta =
                        Metadata.default

                    entryMeta =
                        { defaultMeta | isEntryPoint = True }

                    unqualifiedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "inc"
                                  , { name = "inc"
                                    , metadata = defaultMeta
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "+"
                                        ]
                                    }
                                  )
                                , ( "dec"
                                  , { name = "dec"
                                    , metadata = defaultMeta
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "-"
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata = entryMeta
                                    , implementation =
                                        [ AST.Integer 1
                                        , AST.Word "inc"
                                        , AST.Word "inc"
                                        , AST.Word "dec"
                                        , AST.Integer 2
                                        , AST.Word "="
                                        ]
                                    }
                                  )
                                ]
                        }

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromList
                                [ ( "inc"
                                  , { name = "inc"
                                    , metadata = defaultMeta
                                    , implementation =
                                        [ Integer 1
                                        , BuiltinPlus
                                        ]
                                    }
                                  )
                                , ( "dec"
                                  , { name = "dec"
                                    , metadata = defaultMeta
                                    , implementation =
                                        [ Integer 1
                                        , BuiltinMinus
                                        ]
                                    }
                                  )
                                , ( "main"
                                  , { name = "main"
                                    , metadata = entryMeta
                                    , implementation =
                                        [ Integer 1
                                        , Word "inc"
                                        , Word "inc"
                                        , Word "dec"
                                        , Integer 2
                                        , BuiltinEqual
                                        ]
                                    }
                                  )
                                ]
                        }
                in
                case qualify unqualifiedAst of
                    Err () ->
                        Expect.fail "Did not expect qualification to fail"

                    Ok qualifiedAst ->
                        Expect.equal expectedAst qualifiedAst
        ]
