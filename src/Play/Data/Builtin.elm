module Play.Data.Builtin exposing (..)

import Play.Data.Type as Type exposing (WordType)


type Builtin
    = Plus
    | Minus
    | Equal


wordType : Builtin -> WordType
wordType builtin =
    case builtin of
        Plus ->
            { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }

        Minus ->
            { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }

        Equal ->
            { input = [ Type.Int, Type.Int ], output = [ Type.Int ] }
