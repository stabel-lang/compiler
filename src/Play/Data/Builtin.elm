module Play.Data.Builtin exposing (..)

import Play.Data.Type as Type exposing (WordType)


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


wordType : Builtin -> WordType
wordType builtin =
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
