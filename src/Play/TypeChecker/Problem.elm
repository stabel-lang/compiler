module Play.TypeChecker.Problem exposing
    ( Problem(..)
    , toString
    )

import Play.Data.SourceLocation exposing (SourceLocationRange)
import Play.Data.Type exposing (Type, WordType)
import Set exposing (Set)


type Problem
    = UndeclaredGeneric SourceLocationRange String (Set String)
    | TypeError SourceLocationRange String WordType WordType
    | UnexpectedType SourceLocationRange String Type Type
    | InconsistentWhens SourceLocationRange String
    | MissingTypeAnnotationInRecursiveCallStack SourceLocationRange String


toString : Problem -> String
toString problem =
    case problem of
        _ ->
            "UNKNOWN"
