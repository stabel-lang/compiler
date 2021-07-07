module Stabel.Data.Builtin exposing
    ( Builtin(..)
    , functionType
    )

import Stabel.Data.Type as Type exposing (FunctionType)


type Builtin
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal
    | StackDuplicate
    | StackDrop
    | StackSwap
    | StackRightRotate
    | StackLeftRotate
    | Apply


functionType : Builtin -> FunctionType
functionType builtin =
    case builtin of
        Plus ->
            { input = [ Type.Int, Type.Int ]
            , output = [ Type.Int ]
            }

        Minus ->
            { input = [ Type.Int, Type.Int ]
            , output = [ Type.Int ]
            }

        Multiply ->
            { input = [ Type.Int, Type.Int ]
            , output = [ Type.Int ]
            }

        Divide ->
            { input = [ Type.Int, Type.Int ]
            , output = [ Type.Int ]
            }

        Equal ->
            { input = [ Type.Int, Type.Int ]
            , output = [ Type.Int ]
            }

        StackDuplicate ->
            { input = [ Type.Generic "a" ]
            , output = [ Type.Generic "a", Type.Generic "a" ]
            }

        StackDrop ->
            { input = [ Type.Generic "a" ]
            , output = []
            }

        StackSwap ->
            { input = [ Type.Generic "a", Type.Generic "b" ]
            , output = [ Type.Generic "b", Type.Generic "a" ]
            }

        StackRightRotate ->
            { input = [ Type.Generic "a", Type.Generic "b", Type.Generic "c" ]
            , output = [ Type.Generic "c", Type.Generic "a", Type.Generic "b" ]
            }

        StackLeftRotate ->
            { input = [ Type.Generic "a", Type.Generic "b", Type.Generic "c" ]
            , output = [ Type.Generic "b", Type.Generic "c", Type.Generic "a" ]
            }

        Apply ->
            { input =
                [ Type.StackRange "a"
                , Type.FunctionSignature
                    { input = [ Type.StackRange "a" ]
                    , output = [ Type.StackRange "b" ]
                    }
                ]
            , output = [ Type.StackRange "b" ]
            }
