module Play.Parser exposing
    ( Module
    , Node(..)
    , TopLevelDefinition(..)
    , parse
    )

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , end
        , int
        , keyword
        , loop
        , map
        , spaces
        , succeed
        , symbol
        )
import Set exposing (Set)


type alias Module =
    { ast : List TopLevelDefinition }


type TopLevelDefinition
    = Def String (List String) (List Node)


type Node
    = Symbol String
    | Integer Int


parse : String -> Result (List Parser.DeadEnd) Module
parse source =
    case Parser.run astParser source of
        Ok ast ->
            Ok { ast = ast }

        Err errors ->
            Err errors


keywords : Set String
keywords =
    Set.fromList [ "def" ]


identifier : Parser String
identifier =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = keywords
        }


astParser : Parser (List TopLevelDefinition)
astParser =
    succeed identity
        |. spaces
        |= loop [] astHelp
        |. spaces
        |. end


astHelp : List TopLevelDefinition -> Parser (Step (List TopLevelDefinition) (List TopLevelDefinition))
astHelp revDefs =
    Parser.oneOf
        [ succeed (\def -> Loop (def :: revDefs))
            |= defParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revDefs))
        ]


defParser : Parser TopLevelDefinition
defParser =
    succeed Def
        |. keyword "def"
        |. spaces
        |= identifier
        |. spaces
        |= loop [] argsHelp
        |. spaces
        |= loop [] nodesHelp


argsHelp : List String -> Parser (Step (List String) (List String))
argsHelp revArgs =
    Parser.oneOf
        [ succeed (\arg -> Loop (arg :: revArgs))
            |= identifier
            |. spaces
        , symbol "="
            |> map (\_ -> Done (List.reverse revArgs))
        ]


nodesHelp : List Node -> Parser (Step (List Node) (List Node))
nodesHelp revNodes =
    Parser.oneOf
        [ succeed (\node -> Loop (node :: revNodes))
            |= nodeParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revNodes))
        ]


nodeParser : Parser Node
nodeParser =
    Parser.oneOf
        [ identifier
            |> map Symbol
        , int
            |> map Integer
        ]
