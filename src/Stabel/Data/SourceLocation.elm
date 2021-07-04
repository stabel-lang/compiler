module Stabel.Data.SourceLocation exposing
    ( SourceLocation
    , SourceLocationRange
    , emptyRange
    , extractFromString
    , toString
    )


type alias SourceLocationRange =
    { source : String
    , start : SourceLocation
    , end : SourceLocation
    }


type alias SourceLocation =
    { row : Int
    , col : Int
    }


emptyRange : SourceLocationRange
emptyRange =
    SourceLocationRange ""
        (SourceLocation 0 0)
        (SourceLocation 0 0)


toString : SourceLocation -> String
toString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.col


extractFromString : String -> SourceLocation -> SourceLocation -> String
extractFromString sourceCode startLoc endLoc =
    let
        numPadding =
            endLoc.row
                |> String.fromInt
                |> String.length

        modifyLastLine idx line =
            if idx == endLoc.row then
                if
                    line
                        |> String.left (endLoc.col - 1)
                        |> String.trim
                        |> String.isEmpty
                then
                    ""

                else
                    line

            else
                line
    in
    sourceCode
        |> String.lines
        |> List.indexedMap (\idx line -> ( idx + 1, line ))
        |> List.map (\( idx, line ) -> ( idx, modifyLastLine idx line ))
        |> List.filter (\( idx, _ ) -> idx >= startLoc.row && idx <= endLoc.row)
        |> dropLastEmptyLines
        |> List.map
            (\( idx, line ) ->
                String.padLeft numPadding ' ' (String.fromInt idx)
                    ++ " | "
                    ++ line
            )
        |> String.join "\n"


dropLastEmptyLines : List ( Int, String ) -> List ( Int, String )
dropLastEmptyLines ls =
    ls
        |> List.reverse
        |> dropLastEmptyLinesHelper
        |> List.reverse


dropLastEmptyLinesHelper : List ( Int, String ) -> List ( Int, String )
dropLastEmptyLinesHelper ls =
    case ls of
        [] ->
            []

        ( _, line ) :: rest ->
            if line |> String.trim |> String.isEmpty then
                dropLastEmptyLinesHelper rest

            else
                ls
