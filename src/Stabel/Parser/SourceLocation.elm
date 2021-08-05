module Stabel.Parser.SourceLocation exposing
    ( SourceLocation
    , SourceLocationRange
    , emptyRange
    )


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
