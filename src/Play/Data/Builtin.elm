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
    | Apply


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
            { input = [ Type.Generic "a_dup" ]
            , output = [ Type.Generic "a_dup", Type.Generic "a_dup" ]
            }

        StackDrop ->
            { input = [ Type.Generic "a_drop" ]
            , output = []
            }

        StackSwap ->
            { input = [ Type.Generic "a_swap", Type.Generic "b_swap" ]
            , output = [ Type.Generic "b_swap", Type.Generic "a_swap" ]
            }

        StackRightRotate ->
            { input = [ Type.Generic "a_rot", Type.Generic "b_rot", Type.Generic "c_rot" ]
            , output = [ Type.Generic "c_rot", Type.Generic "a_rot", Type.Generic "b_rot" ]
            }

        StackLeftRotate ->
            { input = [ Type.Generic "a__rot", Type.Generic "b__rot", Type.Generic "c__rot" ]
            , output = [ Type.Generic "b__rot", Type.Generic "c__rot", Type.Generic "a__rot" ]
            }

        Apply ->
            { input = []
            , output = []
            }
