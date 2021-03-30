module Play.Data.PackageName exposing
    ( PackageName
    , fromString
    , invalidPart
    , toString
    )


type PackageName
    = PackageName String String


fromString : String -> Result () PackageName
fromString str =
    case String.split "/" str of
        [ part1, part2 ] ->
            if invalidPart part1 || invalidPart part2 then
                Err ()

            else
                Ok <| PackageName part1 part2

        _ ->
            Err ()


invalidPart : String -> Bool
invalidPart str =
    case String.uncons str of
        Nothing ->
            True

        Just ( first, rest ) ->
            not
                (Char.isLower first
                    && String.all (\c -> Char.isLower c || Char.isDigit c || c == '_') rest
                )


toString : PackageName -> String
toString (PackageName group name) =
    group ++ "/" ++ name
