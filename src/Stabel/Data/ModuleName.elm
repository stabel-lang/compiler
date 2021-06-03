module Stabel.Data.ModuleName exposing
    ( ModuleName
    , fromString
    , toPartStrings
    , toString
    )

import Stabel.Data.PackageName as PackageName


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



-- TODO: missing tests


toPartStrings : ModuleName -> List String
toPartStrings (ModuleName str) =
    String.split "/" str



-- TODO: missing test


toString : ModuleName -> String
toString (ModuleName str) =
    str
