module Stabel.Data.SourceLocation exposing (..)


type alias SourceLocationRange =
    { start : SourceLocation
    , end : SourceLocation
    }


type alias SourceLocation =
    { row : Int
    , col : Int
    }


emptyRange : SourceLocationRange
emptyRange =
    SourceLocationRange
        (SourceLocation 0 0)
        (SourceLocation 0 0)


toString : SourceLocation -> String
toString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.col


extractFromString : String -> Int -> Int -> String
extractFromString sourceCode startLine endLine =
    let
        numPadding =
            endLine
                |> String.fromInt
                |> String.length
    in
    sourceCode
        |> String.lines
        |> List.indexedMap (\idx line -> ( idx + 1, line ))
        |> List.filter (\( idx, _ ) -> idx >= startLine && idx <= endLine)
        |> List.map
            (\( idx, line ) ->
                String.padLeft numPadding ' ' (String.fromInt idx)
                    ++ " | "
                    ++ line
            )
        |> String.join "\n"
