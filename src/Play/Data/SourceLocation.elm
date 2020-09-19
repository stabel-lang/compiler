module Play.Data.SourceLocation exposing (..)


type alias SourceLocationRange =
    { start : SourceLocation
    , end : SourceLocation
    }


type alias SourceLocation =
    { row : Int
    , col : Int
    , offset : Int
    }


emptyRange : SourceLocationRange
emptyRange =
    SourceLocationRange
        (SourceLocation 0 0 0)
        (SourceLocation 0 0 0)


toString : SourceLocation -> String
toString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.col


extractFromString : String -> SourceLocationRange -> String
extractFromString sourceCode range =
    sourceCode
        |> String.slice range.start.offset range.end.offset
        |> String.trim
        |> String.lines
        |> List.indexedMap (\i l -> String.fromInt (range.start.row + i) ++ " | " ++ l)
        |> String.join "\n"
