module Play.Data.PackageMetadata exposing (PackageMetadata)

import Dict exposing (Dict)
import Play.Data.ModuleName as ModuleName exposing (ModuleName)
import Play.Data.PackageName as PackageName exposing (PackageName)
import Play.Data.PackagePath as PackagePath exposing (PackagePath)
import Play.Data.SemanticVersion as SemanticVersion exposing (SemanticVersion)


type alias PackageMetadata =
    { name : PackageName
    , version : SemanticVersion
    , compatibleLanguageVersion : SemanticVersion
    , exposedModules : List ModuleName
    , dependencies : Dict String SemanticVersion
    , packagePaths : List PackagePath
    }
