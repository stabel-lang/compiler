module Test.Parser exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Metadata as Metadata
import Play.Data.SourceLocation exposing (SourceLocation, SourceLocationRange, emptyRange)
import Play.Data.Type as Type
import Play.Parser as AST exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util exposing (addFunctionsForStructs, compile, compileRetainLocations)


suite : Test
suite =
    describe "Parser"
        [ test "Sample program" <|
            \_ ->
                let
                    source =
                        """
                        def: inc
                        : 1 +

                        def: dec
                        type: Int -- Int
                        : 1 -

                        def: main
                        entry: true
                        : 1 inc inc dec 2 =
                        """

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , metadata = Metadata.default
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "inc"
                                            , AST.Word emptyRange "inc"
                                            , AST.Word emptyRange "dec"
                                            , AST.Integer emptyRange 2
                                            , AST.Word emptyRange "="
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err _ ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , describe "Custom data structures"
            [ test "Without members" <|
                \_ ->
                    let
                        source =
                            """
                            deftype: True
                            
                            def: as-int
                            type: True -- Int
                            : 1
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange "True" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "as-int"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "True" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.Integer emptyRange 1
                                                ]
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "With members" <|
                \_ ->
                    let
                        source =
                            """
                            deftype: Person
                            : age Int
                            : jobs Int

                            def: get-age
                            type: Person -- Int
                            : age>
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange
                                        "Person"
                                        []
                                        [ ( "age", Type.Int )
                                        , ( "jobs", Type.Int )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "get-age"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.Word emptyRange "age>"
                                                ]
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Generic members" <|
                \_ ->
                    let
                        source =
                            """
                            deftype: Box a
                            : element a
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange
                                        "Box"
                                        [ "a" ]
                                        [ ( "element", Type.Generic "a" )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType
                                                    [ Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl [ AST.ConstructType "Box" ]
                                      }
                                    , { name = ">element"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ], Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Box" "element" ]
                                      }
                                    , { name = "element>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withVerifiedType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                                    [ Type.Generic "a" ]
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Box" "element" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            ]
        , test "Parser understands generic types" <|
            \_ ->
                let
                    source =
                        """
                        def: over
                        type: a b -- a b a
                        : dup rotate
                        """

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Generic "a", Type.Generic "b" ]
                                                [ Type.Generic "a", Type.Generic "b", Type.Generic "a" ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "dup"
                                            , AST.Word emptyRange "rotate"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err _ ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , describe "Unions and multifunctions"
            [ test "Non-generic" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: Bool
                            : True 
                            : False 

                            deftype: True
                            deftype: False

                            defmulti: to-int
                            when: True
                              drop 1
                            when: False
                              drop 0
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "Bool"
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , CustomTypeDef emptyRange "True" [] []
                                    , CustomTypeDef emptyRange "False" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "to-int"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.Custom "True") []
                                                  , [ AST.Word emptyRange "drop", AST.Integer emptyRange 1 ]
                                                  )
                                                , ( TypeMatch emptyRange (Type.Custom "False") []
                                                  , [ AST.Word emptyRange "drop", AST.Integer emptyRange 0 ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Generic" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: Maybe a
                            : a
                            : Nil

                            deftype: Nil

                            defmulti: if-present
                            when: a
                              !
                            when: Nil
                              drop
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "Maybe"
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.Generic "a") []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (Type.Custom "Nil") []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Generic with generic members" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: MaybeBox a
                            : Box a
                            : Nil

                            deftype: Box a
                            : element a

                            deftype: Nil

                            defmulti: if-present
                            when: (Box a)
                              !
                            when: Nil
                              drop
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "MaybeBox"
                                        [ "a" ]
                                        [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef emptyRange "Box" [ "a" ] [ ( "element", Type.Generic "a" ) ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.CustomGeneric "Box" [ Type.Generic "a" ]) []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (Type.Custom "Nil") []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Generic with generic members (with type annotation)" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: MaybeBox a
                            : Box a
                            : Nil

                            deftype: Box a
                            : element a

                            deftype: Nil

                            defmulti: if-present
                            type: (MaybeBox a) a -- a
                            when: (Box a)
                              !
                            when: Nil
                              drop
                            """

                        expectedAst =
                            { types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "MaybeBox"
                                        [ "a" ]
                                        [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef emptyRange "Box" [ "a" ] [ ( "element", Type.Generic "a" ) ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "MaybeBox" [ Type.Generic "a" ], Type.Generic "a" ]
                                                    [ Type.Generic "a" ]
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Type.CustomGeneric "Box" [ Type.Generic "a" ]) []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (Type.Custom "Nil") []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            ]
        , test "Parser understands quotations" <|
            \_ ->
                let
                    source =
                        """
                        def: apply-to-num
                        type: Int [ Int -- Int ] -- Int
                        : !

                        def: main
                        entry: true
                        : 1 [ 1 + ] apply-to-num
                        """

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.Int
                                                , Type.Quotation { input = [ Type.Int ], output = [ Type.Int ] }
                                                ]
                                                [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Quotation
                                                emptyRange
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                            , AST.Word emptyRange "apply-to-num"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err _ ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , test "Parser understands stack ranges" <|
            \_ ->
                let
                    source =
                        """
                        def: apply-to-num
                        type: a... [ a... -- b... ] -- b...
                        : !

                        def: main
                        entry: true
                        : 1 [ 1 + ] apply-to-num
                        """

                    expectedAst =
                        { types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType
                                                [ Type.StackRange "a"
                                                , Type.Quotation
                                                    { input = [ Type.StackRange "a" ]
                                                    , output = [ Type.StackRange "b" ]
                                                    }
                                                ]
                                                [ Type.StackRange "b" ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Quotation emptyRange
                                                [ AST.Integer emptyRange 1
                                                , AST.Word emptyRange "+"
                                                ]
                                            , AST.Word emptyRange "apply-to-num"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err _ ->
                        Expect.fail "Did not expect parsing to fail"

                    Ok ast ->
                        Expect.equal expectedAst ast
        , describe "Pattern matching"
            [ test "Single match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: zero?
                            when: Int( value 0 )
                              >True
                            : >False
                            """

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        Type.Int
                                                        [ ( "value", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Word emptyRange ">True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Recursive match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: pair?
                            when: List( tail List( tail Nil ) )
                              >True
                            : >False
                            """

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "pair?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (Type.Custom "List")
                                                        [ ( "tail"
                                                          , AST.RecursiveMatch
                                                                (TypeMatch emptyRange
                                                                    (Type.Custom "List")
                                                                    [ ( "tail", AST.LiteralType (Type.Custom "Nil") )
                                                                    ]
                                                                )
                                                          )
                                                        ]
                                                  , [ AST.Word emptyRange ">True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Multiple match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: origo?
                            when: Pair( first 0 second 0 )
                              >True
                            : >False
                            """

                        expectedAst =
                            { types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "origo?"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (Type.Custom "Pair")
                                                        [ ( "first", AST.LiteralInt 0 )
                                                        , ( "second", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Word emptyRange ">True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err _ ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            ]
        , test "Support code comments" <|
            \_ ->
                let
                    expectCompiles code =
                        case compile code of
                            Err _ ->
                                Expect.fail "Did not expect compilation to fail."

                            Ok _ ->
                                Expect.pass
                in
                expectCompiles
                    """
                    # Increments the passed in value by one
                    def: inc
                    type: # as you'd expect
                      Int -- Int
                    : # again, pretty basic
                      1 +

                    # Guess we should also have a decrement op
                    def: # what should we call it?
                      dec
                    # The type decleration isn't actually necessary, but doesnt hurt either
                    type: Int -- Int
                    : 1
                      # + wouldnt work here
                      -

                    # And thats it!
                     # wonder what else we should do...
                    """
        , test "Correct line information" <|
            \_ ->
                let
                    source =
                        """
                        defunion: Bool
                        : True
                        : False

                        deftype: True
                        deftype: False

                        defmulti: from-int
                        type: Int -- Int
                        when: Int( value 0 )
                          >False
                        when: Int
                          >True

                        def: equal
                        : - from-int not

                        defmulti: not
                        when: True
                          >False
                        : >True
                        """

                    -- The ending source location for most definitions now ends where the next definition beings
                    -- This is not what we want (it includes too much white space), but it'll do for now.
                    expectedAst =
                        { types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ UnionTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 2 1 1)
                                        (SourceLocation 6 1 32)
                                    )
                                    "Bool"
                                    []
                                    [ Type.Custom "True"
                                    , Type.Custom "False"
                                    ]
                                , CustomTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 6 1 32)
                                        (SourceLocation 7 1 46)
                                    )
                                    "True"
                                    []
                                    []
                                , CustomTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 7 1 46)
                                        (SourceLocation 9 1 62)
                                    )
                                    "False"
                                    []
                                    []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = ">True"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "True" ]
                                  , implementation = SoloImpl [ ConstructType "True" ]
                                  }
                                , { name = ">False"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withVerifiedType [] [ Type.Custom "False" ]
                                  , implementation = SoloImpl [ ConstructType "False" ]
                                  }
                                , { name = "from-int"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                            |> Metadata.withSourceLocationRange
                                                (SourceLocationRange
                                                    (SourceLocation 9 1 62)
                                                    (SourceLocation 16 1 147)
                                                )
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 11 7 104)
                                                        (SourceLocation 11 21 118)
                                                    )
                                                    Type.Int
                                                    [ ( "value", LiteralInt 0 ) ]
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 12 3 121)
                                                        (SourceLocation 13 1 128)
                                                    )
                                                    ">False"
                                                ]
                                              )
                                            , ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 13 7 134)
                                                        (SourceLocation 14 3 140)
                                                    )
                                                    Type.Int
                                                    []
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 14 3 140)
                                                        (SourceLocation 16 1 147)
                                                    )
                                                    ">True"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "equal"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withSourceLocationRange
                                                (SourceLocationRange
                                                    (SourceLocation 16 1 147)
                                                    (SourceLocation 19 1 176)
                                                )
                                  , implementation =
                                        SoloImpl
                                            [ Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 3 160)
                                                    (SourceLocation 17 5 162)
                                                )
                                                "-"
                                            , Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 5 162)
                                                    (SourceLocation 17 14 171)
                                                )
                                                "from-int"
                                            , Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 14 171)
                                                    (SourceLocation 19 1 176)
                                                )
                                                "not"
                                            ]
                                  }
                                , { name = "not"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withSourceLocationRange
                                                (SourceLocationRange
                                                    (SourceLocation 19 1 176)
                                                    (SourceLocation 23 1 218)
                                                )
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 20 7 196)
                                                        (SourceLocation 21 3 203)
                                                    )
                                                    (Type.Custom "True")
                                                    []
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 21 3 203)
                                                        (SourceLocation 22 1 210)
                                                    )
                                                    ">False"
                                                ]
                                              )
                                            ]
                                            [ Word
                                                (SourceLocationRange
                                                    (SourceLocation 22 3 212)
                                                    (SourceLocation 23 1 218)
                                                )
                                                ">True"
                                            ]
                                  }
                                ]
                        }
                in
                case compileRetainLocations source of
                    Err _ ->
                        Expect.fail "Did not expect compilation to fail."

                    Ok ast ->
                        Expect.equal expectedAst ast
        ]
