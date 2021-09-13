module Test.Parser exposing (..)

import Dict
import Dict.Extra as Dict
import Expect
import Fuzz exposing (Fuzzer)
import Random
import Stabel.Parser as AST exposing (..)
import Stabel.Parser.AssociatedFunctionSignature as AssociatedFunctionSignature
import Stabel.Parser.ModuleDefinition as ModuleDefinition
import Stabel.Parser.SourceLocation
    exposing
        ( SourceLocation
        , SourceLocationRange
        , emptyRange
        )
import Stabel.Parser.Type as AST exposing (..)
import Test exposing (Test, describe, fuzz, test)
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "inc"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "dec"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "-"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.Function emptyRange "inc"
                                            , AST.Function emptyRange "inc"
                                            , AST.Function emptyRange "dec"
                                            , AST.Integer emptyRange 2
                                            , AST.Function emptyRange "="
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "int="
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| AST.LocalRef "Int" [], NotStackRange <| AST.LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| AST.LocalRef "Bool" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "-"
                                            , AST.Function emptyRange "zero?"
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "Functions can have documentation strings" <|
            \_ ->
                let
                    source =
                        """
                        def: int=
                        type: Int Int -- Bool
                        doc: \"\"\"
                        A function that checks if two ints are the same
                        \"\"\"
                        : - zero?
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "int="
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| AST.LocalRef "Int" [], NotStackRange <| AST.LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| AST.LocalRef "Bool" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = "A function that checks if two ints are the same"
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "-"
                                            , AST.Function emptyRange "zero?"
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = ","
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| AST.LocalRef "Int" [], NotStackRange <| AST.LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| AST.LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "+"
                                            ]
                                  }
                                , { name = "add2"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| AST.LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| AST.LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 2
                                            , AST.Function emptyRange ","
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "True"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "as-int"
                                      , typeSignature =
                                            AssociatedFunctionSignature.UserProvided
                                                { input = [ NotStackRange <| LocalRef "True" [] ]
                                                , output = [ NotStackRange <| LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Person"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members =
                                            AST.StructMembers
                                                [ ( "age", LocalRef "Int" [] )
                                                , ( "jobs", LocalRef "Int" [] )
                                                ]
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "get-age"
                                      , typeSignature =
                                            AssociatedFunctionSignature.UserProvided
                                                { input = [ NotStackRange <| LocalRef "Person" [] ]
                                                , output = [ NotStackRange <| LocalRef "Int" [] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.Function emptyRange "age>"
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Box"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.StructMembers
                                                [ ( "element", Generic "a" )
                                                ]
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = ">Box"
                                      , typeSignature =
                                            AssociatedFunctionSignature.Verified
                                                { input = [ NotStackRange <| Generic "a" ]
                                                , output = [ NotStackRange <| LocalRef "Box" [ Generic "a" ] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.ConstructType "Box" ]
                                      }
                                    , { name = ">element"
                                      , typeSignature =
                                            AssociatedFunctionSignature.Verified
                                                { input = [ NotStackRange <| LocalRef "Box" [ Generic "a" ], NotStackRange <| Generic "a" ]
                                                , output = [ NotStackRange <| LocalRef "Box" [ Generic "a" ] ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.SetMember "Box" "element" ]
                                      }
                                    , { name = "element>"
                                      , typeSignature =
                                            AssociatedFunctionSignature.Verified
                                                { input = [ NotStackRange <| LocalRef "Box" [ Generic "a" ] ]
                                                , output = [ NotStackRange <| Generic "a" ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl [ AST.GetMember "Box" "element" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            , test "Documentation" <|
                \_ ->
                    let
                        source =
                            """
                            defstruct: Box a
                            doc: \"\"\"
                            Boxes a value
                            \"\"\"
                            : element a
                            """

                        expectedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Box"
                                      , sourceLocation = emptyRange
                                      , documentation = "Boxes a value"
                                      , generics = [ "a" ]
                                      , members =
                                            AST.StructMembers
                                                [ ( "element", Generic "a" )
                                                ]
                                      }
                                    ]
                            , functions = Dict.empty
                            }
                                |> addFunctionsForStructs
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "over"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| Generic "a", NotStackRange <| Generic "b" ]
                                            , output = [ NotStackRange <| Generic "a", NotStackRange <| Generic "b", NotStackRange <| Generic "a" ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "dup"
                                            , AST.Function emptyRange "rotate"
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Bool"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members =
                                            AST.UnionMembers
                                                [ LocalRef "True" []
                                                , LocalRef "False" []
                                                ]
                                      }
                                    , { name = "True"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    , { name = "False"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "to-int"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "True" []) []
                                                  , [ AST.Function emptyRange "drop", AST.Integer emptyRange 1 ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "False" []) []
                                                  , [ AST.Function emptyRange "drop", AST.Integer emptyRange 0 ]
                                                  )
                                                ]
                                                []
                                      }
                                    ]
                            }
                                |> addFunctionsForStructs
                    in
                    expectAst source expectedAst
            , test "Can have documentation strings" <|
                \_ ->
                    let
                        source =
                            """
                            defunion: Bool
                            doc: \"\"\"
                            Short for boolean
                            \"\"\"
                            : True 
                            : False 

                            defstruct: True
                            defstruct: False

                            defmulti: to-int
                            doc: \"\"\"
                            Converts a boolean into an int
                            \"\"\"
                            : True
                              drop 1
                            : False
                              drop 0
                            """

                        expectedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Bool"
                                      , sourceLocation = emptyRange
                                      , documentation = "Short for boolean"
                                      , generics = []
                                      , members =
                                            AST.UnionMembers
                                                [ LocalRef "True" []
                                                , LocalRef "False" []
                                                ]
                                      }
                                    , { name = "True"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    , { name = "False"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "to-int"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = "Converts a boolean into an int"
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "True" []) []
                                                  , [ AST.Function emptyRange "drop", AST.Integer emptyRange 1 ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "False" []) []
                                                  , [ AST.Function emptyRange "drop", AST.Integer emptyRange 0 ]
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "Maybe"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.UnionMembers
                                                [ Generic "a"
                                                , LocalRef "Nil" []
                                                ]
                                      }
                                    , { name = "Nil"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (Generic "a") []
                                                  , [ AST.Function emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Function emptyRange "drop" ]
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "MaybeBox"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.UnionMembers
                                                [ LocalRef "Box" [ Generic "a" ]
                                                , LocalRef "Nil" []
                                                ]
                                      }
                                    , { name = "Box"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.StructMembers
                                                [ ( "element", Generic "a" ) ]
                                      }
                                    , { name = "Nil"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "Box" [ Generic "a" ]) []
                                                  , [ AST.Function emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Function emptyRange "drop" ]
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types =
                                Dict.fromListBy .name
                                    [ { name = "MaybeBox"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.UnionMembers
                                                [ LocalRef "Box" [ Generic "a" ]
                                                , LocalRef "Nil" []
                                                ]
                                      }
                                    , { name = "Box"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = [ "a" ]
                                      , members =
                                            AST.StructMembers
                                                [ ( "element", Generic "a" ) ]
                                      }
                                    , { name = "Nil"
                                      , sourceLocation = emptyRange
                                      , documentation = ""
                                      , generics = []
                                      , members = AST.StructMembers []
                                      }
                                    ]
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "if-present"
                                      , typeSignature =
                                            AssociatedFunctionSignature.UserProvided
                                                { input = [ NotStackRange <| LocalRef "MaybeBox" [ Generic "a" ], NotStackRange <| Generic "a" ]
                                                , output = [ NotStackRange <| Generic "a" ]
                                                }
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange (LocalRef "Box" [ Generic "a" ]) []
                                                  , [ AST.Function emptyRange "!" ]
                                                  )
                                                , ( TypeMatch emptyRange (LocalRef "Nil" []) []
                                                  , [ AST.Function emptyRange "drop" ]
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input =
                                                [ NotStackRange <| LocalRef "Int" []
                                                , NotStackRange <|
                                                    FunctionType
                                                        { input = [ NotStackRange <| LocalRef "Int" [] ]
                                                        , output = [ NotStackRange <| LocalRef "Int" [] ]
                                                        }
                                                ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.InlineFunction
                                                emptyRange
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "+"
                                                ]
                                            , AST.Function emptyRange "apply-to-num"
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "apply-to-num"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input =
                                                [ StackRange "a"
                                                , NotStackRange <|
                                                    FunctionType
                                                        { input = [ StackRange "a" ]
                                                        , output = [ StackRange "b" ]
                                                        }
                                                ]
                                            , output = [ StackRange "b" ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Function emptyRange "!"
                                            ]
                                  }
                                , { name = "main"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.Integer emptyRange 1
                                            , AST.InlineFunction emptyRange
                                                [ AST.Integer emptyRange 1
                                                , AST.Function emptyRange "+"
                                                ]
                                            , AST.Function emptyRange "apply-to-num"
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "zero?"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch
                                                        emptyRange
                                                        (LocalRef "Int" [])
                                                        [ ( "value", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Function emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Function emptyRange "False" ]
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "pair?"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
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
                                                  , [ AST.Function emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Function emptyRange "False" ]
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
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "origo?"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            MultiImpl
                                                [ ( TypeMatch emptyRange
                                                        (LocalRef "Pair" [])
                                                        [ ( "first", AST.LiteralInt 0 )
                                                        , ( "second", AST.LiteralInt 0 )
                                                        ]
                                                  , [ AST.Function emptyRange "True" ]
                                                  )
                                                ]
                                                [ AST.Function emptyRange "False" ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            ]
        , test "Array" <|
            \_ ->
                let
                    source =
                        """
                        def: some-array
                        : { 1 2 /ext/func fn 5 { 1 2 } }
                        """

                    expectedAst =
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types = Dict.empty
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "some-array"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ AST.ArrayLiteral emptyRange
                                                [ AST.Integer emptyRange 1
                                                , AST.Integer emptyRange 2
                                                , AST.ExternalFunction emptyRange
                                                    [ "ext" ]
                                                    "func"
                                                , AST.Function emptyRange "fn"
                                                , AST.Integer emptyRange 5
                                                , AST.ArrayLiteral emptyRange
                                                    [ AST.Integer emptyRange 1
                                                    , AST.Integer emptyRange 2
                                                    ]
                                                ]
                                            ]
                                  }
                                ]
                        }
                in
                expectAst source expectedAst
        , test "Definition without implementation should be legal" <|
            \_ ->
                expectCompiles
                    """
                    def: somefunc
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
                        { sourceReference = ""
                        , moduleDefinition = ModuleDefinition.Undefined
                        , types =
                            Dict.fromListBy .name
                                [ { name = "Bool"
                                  , sourceLocation =
                                        SourceLocationRange
                                            (SourceLocation 2 1)
                                            (SourceLocation 6 1)
                                  , documentation = ""
                                  , generics = []
                                  , members =
                                        AST.UnionMembers
                                            [ LocalRef "True" []
                                            , LocalRef "False" []
                                            ]
                                  }
                                , { name = "True"
                                  , sourceLocation =
                                        SourceLocationRange
                                            (SourceLocation 6 1)
                                            (SourceLocation 7 1)
                                  , documentation = ""
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                , { name = "False"
                                  , sourceLocation =
                                        SourceLocationRange
                                            (SourceLocation 7 1)
                                            (SourceLocation 9 1)
                                  , documentation = ""
                                  , generics = []
                                  , members = AST.StructMembers []
                                  }
                                ]
                        , functions =
                            Dict.fromListBy .name
                                [ { name = "True"
                                  , typeSignature =
                                        AssociatedFunctionSignature.Verified
                                            { input = []
                                            , output = [ NotStackRange <| LocalRef "True" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation = SoloImpl [ ConstructType "True" ]
                                  }
                                , { name = "False"
                                  , typeSignature =
                                        AssociatedFunctionSignature.Verified
                                            { input = []
                                            , output = [ NotStackRange <| LocalRef "False" [] ]
                                            }
                                  , sourceLocationRange = Nothing
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation = SoloImpl [ ConstructType "False" ]
                                  }
                                , { name = "from-int"
                                  , typeSignature =
                                        AssociatedFunctionSignature.UserProvided
                                            { input = [ NotStackRange <| LocalRef "Int" [] ]
                                            , output = [ NotStackRange <| LocalRef "Int" [] ]
                                            }
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 9 1)
                                                (SourceLocation 16 1)
                                            )
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 11 3)
                                                        (SourceLocation 11 17)
                                                    )
                                                    (LocalRef "Int" [])
                                                    [ ( "value", LiteralInt 0 ) ]
                                              , [ Function
                                                    (SourceLocationRange
                                                        (SourceLocation 12 3)
                                                        (SourceLocation 12 8)
                                                    )
                                                    "False"
                                                ]
                                              )
                                            , ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 13 3)
                                                        (SourceLocation 13 6)
                                                    )
                                                    (LocalRef "Int" [])
                                                    []
                                              , [ Function
                                                    (SourceLocationRange
                                                        (SourceLocation 14 3)
                                                        (SourceLocation 14 7)
                                                    )
                                                    "True"
                                                ]
                                              )
                                            ]
                                            []
                                  }
                                , { name = "equal"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 16 1)
                                                (SourceLocation 19 1)
                                            )
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        SoloImpl
                                            [ Function
                                                (SourceLocationRange
                                                    (SourceLocation 17 3)
                                                    (SourceLocation 17 4)
                                                )
                                                "-"
                                            , Function
                                                (SourceLocationRange
                                                    (SourceLocation 17 5)
                                                    (SourceLocation 17 13)
                                                )
                                                "from-int"
                                            , Function
                                                (SourceLocationRange
                                                    (SourceLocation 17 14)
                                                    (SourceLocation 17 17)
                                                )
                                                "not"
                                            ]
                                  }
                                , { name = "not"
                                  , typeSignature = AssociatedFunctionSignature.NotProvided
                                  , sourceLocationRange =
                                        Just
                                            (SourceLocationRange
                                                (SourceLocation 19 1)
                                                (SourceLocation 23 1)
                                            )
                                  , documentation = ""
                                  , aliases = Dict.empty
                                  , imports = Dict.empty
                                  , implementation =
                                        MultiImpl
                                            [ ( TypeMatch
                                                    (SourceLocationRange
                                                        (SourceLocation 20 3)
                                                        (SourceLocation 20 7)
                                                    )
                                                    (LocalRef "True" [])
                                                    []
                                              , [ Function
                                                    (SourceLocationRange
                                                        (SourceLocation 21 3)
                                                        (SourceLocation 21 8)
                                                    )
                                                    "False"
                                                ]
                                              )
                                            ]
                                            [ Function
                                                (SourceLocationRange
                                                    (SourceLocation 22 7)
                                                    (SourceLocation 22 11)
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
        , describe "Strings" <|
            let
                expectParseString input output =
                    let
                        source =
                            """
                            def: str
                            : """ ++ "\"" ++ input ++ "\""

                        expectedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "str"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.StringLiteral emptyRange output
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            in
            [ test "Simple case" <|
                \_ ->
                    expectParseString
                        "This is a test"
                        "This is a test"
            , test "Empty string" <|
                \_ ->
                    expectParseString
                        ""
                        ""
            , test "Newline escape sequence" <|
                \_ ->
                    expectParseString
                        "This is a test with\\n a newline"
                        "This is a test with\n a newline"
            , test "Tab escape sequence" <|
                \_ ->
                    expectParseString
                        "This is a test with\\t a tab"
                        "This is a test with\t a tab"
            , test "Backslash escape sequence" <|
                \_ ->
                    expectParseString
                        "This is a test with\\\\ a backslash"
                        "This is a test with\\ a backslash"
            , test "Double-quote escape sequence" <|
                \_ ->
                    expectParseString
                        "This is a \\\"test\\\" with a double quote"
                        "This is a \"test\" with a double quote"
            ]
        , describe "Multiline strings" <|
            let
                expectParseString input output =
                    let
                        source =
                            """
                            def: str
                            : """ ++ "\"\"\"" ++ input ++ "\"\"\""

                        expectedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "str"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.StringLiteral emptyRange output
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            in
            [ test "Simple case" <|
                \_ ->
                    expectParseString
                        "This is a test"
                        "This is a test"
            , test "Empty multiline string" <|
                \_ ->
                    expectParseString
                        ""
                        ""
            , test "There are no escape sequences" <|
                \_ ->
                    expectParseString
                        "This is a test with\\n a newline escape sequence"
                        "This is a test with\\n a newline escape sequence"
            , test "Actual multiline" <|
                \_ ->
                    expectParseString
                        "\n    This is a test\n      with some indentation\n    Let's see how it turns out\n"
                        "This is a test\n  with some indentation\nLet's see how it turns out"
            , test "Above test using Elm's multiline" <|
                \_ ->
                    expectParseString
                        """
                        This is a test
                          with some indentation
                        Let's see how it turns out
                        """
                        "This is a test\n  with some indentation\nLet's see how it turns out"
            ]
        , describe "Integers" <|
            let
                expectParseInt input output =
                    let
                        source =
                            """
                            def: test
                            : """ ++ input

                        expectedAst =
                            { sourceReference = ""
                            , moduleDefinition = ModuleDefinition.Undefined
                            , types = Dict.empty
                            , functions =
                                Dict.fromListBy .name
                                    [ { name = "test"
                                      , typeSignature = AssociatedFunctionSignature.NotProvided
                                      , sourceLocationRange = Nothing
                                      , documentation = ""
                                      , aliases = Dict.empty
                                      , imports = Dict.empty
                                      , implementation =
                                            SoloImpl
                                                [ AST.Integer emptyRange output
                                                ]
                                      }
                                    ]
                            }
                    in
                    expectAst source expectedAst
            in
            [ fuzz positiveIntFuzzer "Positive int" <|
                \num ->
                    expectParseInt (String.fromInt num) num
            , fuzz negativeIntFuzzer "Negative ints" <|
                \num ->
                    expectParseInt (String.fromInt (num * -1) ++ "-") num
            , test "can contain underscores as seperators" <|
                \_ ->
                    expectParseInt "10_000" 10000
            ]
        ]


positiveIntFuzzer : Fuzzer Int
positiveIntFuzzer =
    Fuzz.intRange 0 Random.maxInt


negativeIntFuzzer : Fuzzer Int
negativeIntFuzzer =
    Fuzz.intRange Random.minInt -1
