module Play.Parser exposing
    ( Module
    , Node(..)
    , TopLevelDefinition(..)
    , parse
    )


type alias Module =
    { ast : List TopLevelDefinition }


type TopLevelDefinition
    = Def String (List String) (List Node)


type Node
    = Symbol String
    | Integer Int


parse : String -> Module
parse source =
    { ast = [] }
