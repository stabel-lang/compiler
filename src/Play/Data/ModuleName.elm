module Play.Data.ModuleName exposing
    ( ModuleName
    , fromString
    )

import Play.Data.PackageName as PackageName


type ModuleName
    = ModuleName String


fromString : String -> Result () ModuleName
fromString str =
    let
        parts =
            String.split "/" str
    in
    if List.any PackageName.invalidPart parts then
        Err ()

    else
        Ok <| ModuleName <| String.join "/" parts
