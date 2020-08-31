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
