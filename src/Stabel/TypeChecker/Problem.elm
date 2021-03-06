module Stabel.TypeChecker.Problem exposing
    ( Problem(..)
    , sourceLocationRef
    , toString
    )

import Stabel.Data.SourceLocation as SourceLocation exposing (SourceLocationRange)
import Stabel.Data.Type as Type exposing (FunctionType, Type)


type Problem
    = UndeclaredGeneric SourceLocationRange String
    | TypeError SourceLocationRange String FunctionType FunctionType
    | UnexpectedType SourceLocationRange String Type Type
    | InconsistentWhens SourceLocationRange String
    | MissingTypeAnnotationInRecursiveCallStack SourceLocationRange String
    | InexhaustiveMultiFunction SourceLocationRange (List (List Type))
    | BadEntryPoint SourceLocationRange String FunctionType FunctionType
    | BadArrayElement SourceLocationRange FunctionType


toString : String -> Problem -> String
toString source problem =
    case problem of
        UndeclaredGeneric range generic ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Generic variable '"
                ++ generic
                ++ "' needs to be declared."

        TypeError range name actual expected ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "The type of '"
                ++ name
                ++ "' is specified to be:\n\n"
                ++ Type.functionTypeToString actual
                ++ "\n\nHowever, it seems that the actual type is:\n\n"
                ++ Type.functionTypeToString expected

        UnexpectedType range name actual expected ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "Found a problem in the implementation of '"
                ++ name
                ++ "'\n\nExpected:\n\n"
                ++ Type.toDisplayString expected
                ++ "\n\nActual:\n\n"
                ++ Type.toDisplayString actual

        InconsistentWhens range name ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "The branches of '"
                ++ name
                ++ "' do not all have the same type."

        MissingTypeAnnotationInRecursiveCallStack range name ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "We require a type annotation for '"
                ++ name
                ++ "' as we're unable to infer the type of a recursive call."

        InexhaustiveMultiFunction range missingTypes ->
            let
                formatTypePattern tp =
                    String.join " -> " (List.map Type.toDisplayString tp)
            in
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "This multi-function doesn't handle all potential patterns. Missing patterns for:\n\n"
                ++ String.join "\n" (List.map formatTypePattern missingTypes)

        BadEntryPoint range name actual expected ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "In order to be called from the command line, the type of '"
                ++ name
                ++ "' needs to be:\n\n"
                ++ Type.functionTypeToString actual
                ++ "\n\nHowever, it seems that the actual type is:\n\n"
                ++ Type.functionTypeToString expected

        BadArrayElement range actual ->
            ">> "
                ++ range.source
                ++ "\n\n"
                ++ SourceLocation.extractFromString source range.start range.end
                ++ "\n\n"
                ++ "An Array can only contain elements of type ( -- a ), but found an element with type:"
                ++ "\n\n"
                ++ Type.functionTypeToString actual


sourceLocationRef : Problem -> String
sourceLocationRef problem =
    case problem of
        UndeclaredGeneric range _ ->
            range.source

        TypeError range _ _ _ ->
            range.source

        UnexpectedType range _ _ _ ->
            range.source

        InconsistentWhens range _ ->
            range.source

        MissingTypeAnnotationInRecursiveCallStack range _ ->
            range.source

        InexhaustiveMultiFunction range _ ->
            range.source

        BadEntryPoint range _ _ _ ->
            range.source

        BadArrayElement range _ ->
            range.source
