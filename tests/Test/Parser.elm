module Test.Parser exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Play.Data.Metadata as Metadata
import Play.Data.Type as Type
import Play.Parser as AST exposing (..)
import String.Extra as String
import Test exposing (Test, describe, test)


compile : String -> Result () AST
compile str =
    run <| String.unindent str


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
                                            [ AST.Integer 1
                                            , AST.Word "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.withType [ Type.Int ] [ Type.Int ]
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer 1
                                            , AST.Word "-"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer 1
                                            , AST.Word "inc"
                                            , AST.Word "inc"
                                            , AST.Word "dec"
                                            , AST.Integer 2
                                            , AST.Word "="
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err () ->
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
                                    [ CustomTypeDef "True" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">True"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "True" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "True" ]
                                      }
                                    , { name = "as-int"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "True" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.Integer 1
                                                ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                    [ CustomTypeDef "Person"
                                        []
                                        [ ( "age", Type.Int )
                                        , ( "jobs", Type.Int )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Person"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Int, Type.Int ] [ Type.Custom "Person" ]
                                      , implementation =
                                            SoloImpl [ AST.ConstructType "Person" ]
                                      }
                                    , { name = ">age"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Person" "age" ]
                                      }
                                    , { name = ">jobs"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person", Type.Int ] [ Type.Custom "Person" ]
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Person" "jobs" ]
                                      }
                                    , { name = "age>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Person" "age" ]
                                      }
                                    , { name = "jobs>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Person" "jobs" ]
                                      }
                                    , { name = "get-age"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Custom "Person" ] [ Type.Int ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.Word "age>"
                                                ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                    [ CustomTypeDef "Box"
                                        [ "a" ]
                                        [ ( "element", Type.Generic "a" )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl [ AST.ConstructType "Box" ]
                                      }
                                    , { name = ">element"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ], Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Box" "element" ]
                                      }
                                    , { name = "element>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                                    [ Type.Generic "a" ]
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Box" "element" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                            [ AST.Word "dup"
                                            , AST.Word "rotate"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err () ->
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
                                    [ UnionTypeDef "Bool"
                                        []
                                        [ Type.Custom "True"
                                        , Type.Custom "False"
                                        ]
                                    , CustomTypeDef "True" [] []
                                    , CustomTypeDef "False" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">True"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "True" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "True"
                                                ]
                                      }
                                    , { name = ">False"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "False" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "False"
                                                ]
                                      }
                                    , { name = "to-int"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.Custom "True") []
                                                  , [ AST.Word "drop", AST.Integer 1 ]
                                                  )
                                                , ( TypeMatch (Type.Custom "False") []
                                                  , [ AST.Word "drop", AST.Integer 0 ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                    [ UnionTypeDef "Maybe"
                                        [ "a" ]
                                        [ Type.Generic "a"
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Nil"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "Nil" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "Nil"
                                                ]
                                      }
                                    , { name = "if-present"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.Generic "a") []
                                                  , [ AST.Word "!" ]
                                                  )
                                                , ( TypeMatch (Type.Custom "Nil") []
                                                  , [ AST.Word "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                    [ UnionTypeDef "MaybeBox"
                                        [ "a" ]
                                        [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef "Box" [ "a" ] [ ( "element", Type.Generic "a" ) ]
                                    , CustomTypeDef "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">element"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    , Type.Generic "a"
                                                    ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.SetMember "Box" "element"
                                                ]
                                      }
                                    , { name = "element>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    ]
                                                    [ Type.Generic "a"
                                                    ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.GetMember "Box" "element"
                                                ]
                                      }
                                    , { name = ">Nil"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "Nil" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "Nil"
                                                ]
                                      }
                                    , { name = "if-present"
                                      , metadata = Metadata.default
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.CustomGeneric "Box" [ Type.Generic "a" ]) []
                                                  , [ AST.Word "!" ]
                                                  )
                                                , ( TypeMatch (Type.Custom "Nil") []
                                                  , [ AST.Word "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                    [ UnionTypeDef "MaybeBox"
                                        [ "a" ]
                                        [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                        , Type.Custom "Nil"
                                        ]
                                    , CustomTypeDef "Box" [ "a" ] [ ( "element", Type.Generic "a" ) ]
                                    , CustomTypeDef "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [ Type.Generic "a" ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ] ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "Box"
                                                ]
                                      }
                                    , { name = ">element"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    , Type.Generic "a"
                                                    ]
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.SetMember "Box" "element"
                                                ]
                                      }
                                    , { name = "element>"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "Box" [ Type.Generic "a" ]
                                                    ]
                                                    [ Type.Generic "a"
                                                    ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.GetMember "Box" "element"
                                                ]
                                      }
                                    , { name = ">Nil"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType [] [ Type.Custom "Nil" ]
                                      , implementation =
                                            SoloImpl
                                                [ AST.ConstructType "Nil"
                                                ]
                                      }
                                    , { name = "if-present"
                                      , metadata =
                                            Metadata.default
                                                |> Metadata.withType
                                                    [ Type.CustomGeneric "MaybeBox" [ Type.Generic "a" ], Type.Generic "a" ]
                                                    [ Type.Generic "a" ]
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch (Type.CustomGeneric "Box" [ Type.Generic "a" ]) []
                                                  , [ AST.Word "!" ]
                                                  )
                                                , ( TypeMatch (Type.Custom "Nil") []
                                                  , [ AST.Word "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                            [ AST.Word "!"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer 1
                                            , AST.Quotation
                                                [ AST.Integer 1
                                                , AST.Word "+"
                                                ]
                                            , AST.Word "apply-to-num"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err () ->
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
                                            [ AST.Word "!"
                                            ]
                                  }
                                , { name = "main"
                                  , metadata =
                                        Metadata.default
                                            |> Metadata.asEntryPoint
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer 1
                                            , AST.Quotation
                                                [ AST.Integer 1
                                                , AST.Word "+"
                                                ]
                                            , AST.Word "apply-to-num"
                                            ]
                                  }
                                ]
                        }
                in
                case compile source of
                    Err () ->
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
                                                [ ( TypeMatch Type.Int [ ( "value", AST.LiteralInt 0 ) ], [ AST.Word ">True" ] )
                                                ]
                                                [ AST.Word ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                                [ ( TypeMatch (Type.Custom "List")
                                                        [ ( "tail"
                                                          , AST.RecursiveMatch
                                                                (TypeMatch (Type.Custom "List")
                                                                    [ ( "tail", AST.LiteralType (Type.Custom "Nil") )
                                                                    ]
                                                                )
                                                          )
                                                        ]
                                                  , [ AST.Word ">True" ]
                                                  )
                                                ]
                                                [ AST.Word ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
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
                                                [ ( TypeMatch (Type.Custom "Pair")
                                                        [ ( "first", AST.LiteralInt 0 )
                                                        , ( "second", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Word ">True" ]
                                                  )
                                                ]
                                                [ AST.Word ">False" ]
                                      }
                                    ]
                            }
                    in
                    case compile source of
                        Err () ->
                            Expect.fail "Did not expect parsing to fail"

                        Ok ast ->
                            Expect.equal expectedAst ast
            , test "Syntax error" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: origo?
                            when: Pair( 0 0 )
                              >True
                            : >False
                            """
                    in
                    case compile source of
                        Err () ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Did not expect parsing to succeed"
            ]
        , test "Support code comments" <|
            \_ ->
                let
                    expectCompiles code =
                        case compile code of
                            Err () ->
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
        ]
