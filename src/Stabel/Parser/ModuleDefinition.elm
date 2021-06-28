module Stabel.Parser.ModuleDefinition exposing
    ( Definition
    , ModuleDefinition(..)
    , definition
    , emptyDefinition
    )

import Dict exposing (Dict)
import Set exposing (Set)


type ModuleDefinition
    = Undefined
    | Defined Definition


type alias Definition =
    { aliases : Dict String String
    , imports : Dict String (List String)
    , exposes : Set String
    }


emptyDefinition : Definition
emptyDefinition =
    { aliases = Dict.empty
    , imports = Dict.empty
    , exposes = Set.empty
    }


definition : ModuleDefinition -> Definition
definition mod =
    case mod of
        Undefined ->
            emptyDefinition

        Defined def ->
            def
