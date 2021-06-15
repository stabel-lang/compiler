module Test.Parser exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Stabel.Data.SourceLocation
    exposing
        ( SourceLocation
        , SourceLocationRange
        , emptyRange
        )
import Stabel.Parser as AST exposing (..)
import Test exposing (Test, describe, test)
import Test.Parser.Util
    exposing
        ( addFunctionsForStructs
        , compileRetainLocations
        , expectAst
        , expectCompiles
        )


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
                        : 1 inc inc dec 2 =
                        """

                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , typeSignature =
                                        UserProvided
                                            { input = [ LocalRef "Int" [] ]
                                            , output = [ LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Word emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
                expectAst source expectedAst
        , test "Multi args and type signature" <|
            \_ ->
                let
                    source =
                        """
                        def: int=
                        type: Int Int -- Bool
                        : - zero?
                        """

                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "int="
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "Int" [], AST.LocalRef "Int" [] ]
                                            , output = [ AST.LocalRef "Bool" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "-"
                                            , AST.Word emptyRange "zero?"
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test ", is a valid fn name" <|
            \_ ->
                let
                    source =
                        """
                        def: ,
                        type: Int Int -- Int
                        : +

                        def: add2
                        type: Int -- Int
                        : 2 ,
                        """

                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = ","
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "Int" [], AST.LocalRef "Int" [] ]
                                            , output = [ AST.LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "+"
                                            ]
                                  }
                                , { name = "add2"
                                  , typeSignature =
                                        AST.UserProvided
                                            { input = [ AST.LocalRef "Int" [] ]
                                            , output = [ AST.LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 2
                                            , AST.Word emptyRange ","
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , describe "Custom data structures"
            [ test "Without members" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: True
                            
                            def: as-int
                            type: True -- Int
                            : 1
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange "True" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "as-int"
                                      , typeSignature =
                                            UserProvided
                                                { input = [ LocalRef "True" [] ]
                                                , output = [ LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.Integer emptyRange 1
                                                ]
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "With members" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: Person
                            : age Int
                            : jobs Int

                            def: get-age
                            type: Person -- Int
                            : age>
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange
                                        "Person"
                                        []
                                        [ ( "age", LocalRef "Int" [] )
                                        , ( "jobs", LocalRef "Int" [] )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "get-age"
                                      , typeSignature =
                                            UserProvided
                                                { input = [ LocalRef "Person" [] ]
                                                , output = [ LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.Word emptyRange "age>"
                                                ]
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "Generic members" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: Box a
                            : element a
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ CustomTypeDef emptyRange
                                        "Box"
                                        [ "a" ]
                                        [ ( "element", Generic "a" )
                                        ]
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , typeSignature =
                                            Verified
                                                { input = [ Generic "a" ]
                                                , output = [ LocalRef "Box" [ Generic "a" ] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.ConstructType "Box" ]
                                      }
                                    , { name = ">element"
                                      , typeSignature =
                                            Verified
                                                { input = [ LocalRef "Box" [ Generic "a" ], Generic "a" ]
                                                , output = [ LocalRef "Box" [ Generic "a" ] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Box" "element" ]
                                      }
                                    , { name = "element>"
                                      , typeSignature =
                                            Verified
                                                { input = [ LocalRef "Box" [ Generic "a" ] ]
                                                , output = [ Generic "a" ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Box" "element" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
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
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , typeSignature =
                                        UserProvided
                                            { input = [ Generic "a", Generic "b" ]
                                            , output = [ Generic "a", Generic "b", Generic "a" ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "dup"
                                            , AST.Word emptyRange "rotate"
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , describe "Unions and multifunctions"
            [ test "Non-generic" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: Bool
                            : True 
                            : False 

                            defstruct: True
                            defstruct: False

                            defmulti: to-int
                            : True
                              drop 1
                            : False
                              drop 0
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "Bool"
                                        []
                                        [ LocalRef "True" []
                                        , LocalRef "False" []
                                        ]
                                    , CustomTypeDef emptyRange "True" [] []
                                    , CustomTypeDef emptyRange "False" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "to-int"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "True" []) []
                                                  , [ AST.Word emptyRange "drop", AST.Integer emptyRange 1 ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "False" []) []
                                                  , [ AST.Word emptyRange "drop", AST.Integer emptyRange 0 ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "Generic" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: Maybe a
                            : a
                            : Nil

                            defstruct: Nil

                            defmulti: if-present
                            : a
                              !
                            : Nil
                              drop
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "Maybe"
                                        [ "a" ]
                                        [ Generic "a"
                                        , LocalRef "Nil" []
                                        ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Generic "a") []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "Generic with generic members" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: MaybeBox a
                            : Box a
                            : Nil

                            defstruct: Box a
                            : element a

                            defstruct: Nil

                            defmulti: if-present
                            : (Box a)
                              !
                            : Nil
                              drop
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "MaybeBox"
                                        [ "a" ]
                                        [ LocalRef "Box" [ Generic "a" ]
                                        , LocalRef "Nil" []
                                        ]
                                    , CustomTypeDef emptyRange "Box" [ "a" ] [ ( "element", Generic "a" ) ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "Box" [ Generic "a" ]) []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "Generic with generic members (with type annotation)" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: MaybeBox a
                            : Box a
                            : Nil

                            defstruct: Box a
                            : element a

                            defstruct: Nil

                            defmulti: if-present
                            type: (MaybeBox a) a -- a
                            : (Box a)
                              !
                            : Nil
                              drop
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types =
                                Dict.fromListBy AST.typeDefinitionName
                                    [ UnionTypeDef emptyRange
                                        "MaybeBox"
                                        [ "a" ]
                                        [ LocalRef "Box" [ Generic "a" ]
                                        , LocalRef "Nil" []
                                        ]
                                    , CustomTypeDef emptyRange "Box" [ "a" ] [ ( "element", Generic "a" ) ]
                                    , CustomTypeDef emptyRange "Nil" [] []
                                    ]
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature =
                                            UserProvided
                                                { input = [ LocalRef "MaybeBox" [ Generic "a" ], Generic "a" ]
                                                , output = [ Generic "a" ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "Box" [ Generic "a" ]) []
                                                  , [ AST.Word emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Word emptyRange "drop" ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
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
                        : 1 [ 1 + ] apply-to-num
                        """

                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , typeSignature =
                                        UserProvided
                                            { input =
                                                [ LocalRef "Int" []
                                                , QuotationType
                                                    { input = [ LocalRef "Int" [] ]
                                                    , output = [ LocalRef "Int" [] ]
                                                    }
                                                ]
                                            , output = [ LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
                expectAst source expectedAst
        , test "Parser understands stack ranges" <|
            \_ ->
                let
                    source =
                        """
                        def: apply-to-num
                        type: a... [ a... -- b... ] -- b...
                        : !

                        def: main
                        : 1 [ 1 + ] apply-to-num
                        """

                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types = Dict.empty
                        , words =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , typeSignature =
                                        UserProvided
                                            { input =
                                                [ StackRange "a"
                                                , QuotationType
                                                    { input = [ StackRange "a" ]
                                                    , output = [ StackRange "b" ]
                                                    }
                                                ]
                                            , output = [ StackRange "b" ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Word emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
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
                expectAst source expectedAst
        , describe "Pattern matching"
            [ test "Single match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: zero?
                            : Int( value 0 )
                              True
                            else: False
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch
                                                        emptyRange
                                                        (LocalRef "Int" [])
                                                        [ ( "value", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Word emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "False" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            , test "Recursive match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: pair?
                            : List( tail List( tail Nil ) )
                              True
                            else: False
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "pair?"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (LocalRef "List" [])
                                                        [ ( "tail"
                                                          , AST.RecursiveMatch
                                                                (TypeMatch emptyRange
                                                                    (LocalRef "List" [])
                                                                    [ ( "tail", AST.LiteralType (LocalRef "Nil" []) )
                                                                    ]
                                                                )
                                                          )
                                                        ]
                                                  , [ AST.Word emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "False" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            , test "Multiple match" <|
                \_ ->
                    let
                        source =
                            """
                            defmulti: origo?
                            : Pair( first 0 second 0 )
                              True
                            else: False
                            """

                        expectedAst =
                            { moduleDefinition = AST.emptyModuleDefinition
                            , types = Dict.empty
                            , words =
                                Dict.fromListBy .name
                                    [ { name = "origo?"
                                      , typeSignature = NotProvided
                                      , sourceLocationRange = Nothing
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (LocalRef "Pair" [])
                                                        [ ( "first", AST.LiteralInt 0 )
                                                        , ( "second", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Word emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Word emptyRange "False" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            ]
        , test "Definition without implementation should be legal" <|
            \_ ->
                expectCompiles
                    """
                    def: someword
                    """
        , test "Support code comments" <|
            \_ ->
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

                        defstruct: True
                        defstruct: False

                        defmulti: from-int
                        type: Int -- Int
                        : Int( value 0 )
                          False
                        : Int
                          True

                        def: equal
                        : - from-int not

                        defmulti: not
                        : True
                          False
                        else: True
                        """

                    -- The ending source location for most definitions now ends where the next definition beings
                    -- This is not what we want (it includes too much white space), but it'll do for now.
                    expectedAst =
                        { moduleDefinition = AST.emptyModuleDefinition
                        , types =
                            Dict.fromListBy AST.typeDefinitionName
                                [ UnionTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 2 1 1)
                                        (SourceLocation 6 1 32)
                                    )
                                    "Bool"
                                    []
                                    [ LocalRef "True" []
                                    , LocalRef "False" []
                                    ]
                                , CustomTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 6 1 32)
                                        (SourceLocation 7 1 48)
                                    )
                                    "True"
                                    []
                                    []
                                , CustomTypeDef
                                    (SourceLocationRange
                                        (SourceLocation 7 1 48)
                                        (SourceLocation 9 1 66)
                                    )
                                    "False"
                                    []
                                    []
                                ]
                        , words =
                            Dict.fromListBy .name
                                [ { name = "True"
                                  , typeSignature =
                                        Verified
                                            { input = []
                                            , output = [ LocalRef "True" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation = SoloImpl [ ConstructType "True" ]
                                  }
                                , { name = "False"
                                  , typeSignature =
                                        Verified
                                            { input = []
                                            , output = [ LocalRef "False" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation = SoloImpl [ ConstructType "False" ]
                                  }
                                , { name = "from-int"
                                  , typeSignature =
                                        UserProvided
                                            { input = [ LocalRef "Int" [] ]
                                            , output = [ LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 9 1 66)
                                                (SourceLocation 16 1 141)
                                            )
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 11 3 104)
                                                        (SourceLocation 11 17 118)
                                                    )
                                                    (LocalRef "Int" [])
                                                    [ ( "value", LiteralInt 0 ) ]
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 12 3 121)
                                                        (SourceLocation 13 1 127)
                                                    )
                                                    "False"
                                                ]
                                              )
                                            , ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 13 3 129)
                                                        (SourceLocation 14 3 135)
                                                    )
                                                    (LocalRef "Int" [])
                                                    []
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 14 3 135)
                                                        (SourceLocation 16 1 141)
                                                    )
                                                    "True"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "equal"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 16 1 141)
                                                (SourceLocation 19 1 170)
                                            )
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 3 154)
                                                    (SourceLocation 17 5 156)
                                                )
                                                "-"
                                            , Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 5 156)
                                                    (SourceLocation 17 14 165)
                                                )
                                                "from-int"
                                            , Word
                                                (SourceLocationRange
                                                    (SourceLocation 17 14 165)
                                                    (SourceLocation 19 1 170)
                                                )
                                                "not"
                                            ]
                                  }
                                , { name = "not"
                                  , typeSignature = NotProvided
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 19 1 170)
                                                (SourceLocation 23 1 210)
                                            )
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 20 3 186)
                                                        (SourceLocation 21 3 193)
                                                    )
                                                    (LocalRef "True" [])
                                                    []
                                              , [ Word
                                                    (SourceLocationRange
                                                        (SourceLocation 21 3 193)
                                                        (SourceLocation 22 1 199)
                                                    )
                                                    "False"
                                                ]
                                              )
                                            ]
                                            [ Word
                                                (SourceLocationRange
                                                    (SourceLocation 22 7 205)
                                                    (SourceLocation 23 1 210)
                                                )
                                                "True"
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
