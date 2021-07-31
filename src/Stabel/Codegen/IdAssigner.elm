module Stabel.Codegen.IdAssigner exposing
    ( IdAssigner
    , assignId
    , empty
    )

import Dict exposing (Dict)


type alias IdAssigner =
    { nextId : Int
    , cache : Dict String Int
    }


empty : Int -> IdAssigner
empty startingId =
    { nextId = startingId
    , cache = Dict.empty
    }


assignId : String -> IdAssigner -> ( Int, IdAssigner )
assignId name assign =
    case Dict.get name assign.cache of
        Just id ->
            ( id, assign )

        Nothing ->
            ( assign.nextId
            , { assign
                | nextId = assign.nextId + 1
                , cache = Dict.insert name assign.nextId assign.cache
              }
            )
