module Stabel.TypeChecker.Problem exposing
    ( Problem(..)
    , toString
    )

import Set exposing (Set)
import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Stabel.Data.Type as Type exposing (Type, WordType)


type Problem
    = UndeclaredGeneric SourceLocationRange String (Set String)
    | TypeError SourceLocationRange String WordType WordType
    | UnexpectedType SourceLocationRange String Type Type
    | InconsistentWhens SourceLocationRange String
    | MissingTypeAnnotationInRecursiveCallStack SourceLocationRange String
    | InexhaustiveMultiWord SourceLocationRange (List (List Type))


toString : String -> Problem -> String
toString source problem =
    case problem of
        UndeclaredGeneric range generic _ ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "Generic variable '"
                ++ generic
                ++ "' needs to be declared."

        TypeError range name actual expected ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "The type of '"
                ++ name
                ++ "' is specified to be:\n\n"
                ++ Type.wordTypeToString actual
                ++ "\n\nHowever, it seems that the actual type is:\n\n"
                ++ Type.wordTypeToString expected

        UnexpectedType range name actual expected ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "Found a problem in the implementation of '"
                ++ name
                ++ "'.\n\nExpected:\n\n"
                ++ Type.toDisplayString expected
                ++ "\n\nActual:\n"
                ++ Type.toDisplayString actual

        InconsistentWhens range name ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "The branches of '"
                ++ name
                ++ "' do not all have the same type."

        MissingTypeAnnotationInRecursiveCallStack range name ->
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "We require a type annotation for '"
                ++ name
                ++ "' as we're unable to infer the type of a recursive call."

        InexhaustiveMultiWord range missingTypes ->
            let
                formatTypePattern tp =
                    String.join " -> " (List.map Type.toDisplayString tp)
            in
            SourceLocation.extractFromString source range
                ++ "\n\n"
                ++ "This multiword doesn't handle all potential patterns. Missing patterns for:\n\n"
                ++ String.join "\n" (List.map formatTypePattern missingTypes)
