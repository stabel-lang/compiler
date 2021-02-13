module Play.Data.PackagePath exposing
    ( PackagePath(..)
    , fromString
    )


type PackagePath
    = Directory String
    | AllDirectoriesInDirectory String


fromString : String -> PackagePath
fromString str =
    let
        ctor =
            if String.endsWith "*" str then
                AllDirectoriesInDirectory

            else
                Directory

        charsToDrop =
            if String.endsWith "/*" str then
                2

            else if String.endsWith "*" str then
                1

            else if String.endsWith "/" str then
                1

            else
                0
    in
    ctor <| String.dropRight charsToDrop str
