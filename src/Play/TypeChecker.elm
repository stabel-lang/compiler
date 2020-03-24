module Play.TypeChecker exposing (..)

import Dict exposing (Dict)
import Play.Parser as PAST
import Play.Qualifier as Qualifier


type alias TypedDefinitions =
    { name : String
    , type_ : WordType
    , metadata : List ( String, List PAST.AstNode )
    , implementation : List AstNode
    }


type Type
    = IntType


type alias WordType =
    { input : List Type
    , output : List Type
    }


type AstNode
    = IntLiteral Int
    | Word String WordType
    | BuiltinPlus
    | BuiltinMinus
    | BuiltinEqual


type alias Context =
    { knownWords : Dict String WordType }


initContext : Context
initContext =
    { knownWords = Dict.empty }


typeCheck : List Qualifier.Definition -> Result () (List TypedDefinitions)
typeCheck ast =
    typeCheckHelper initContext ast


typeCheckHelper : Context -> List Qualifier.Definition -> Result () (List TypedDefinitions)
typeCheckHelper context ast =
    Ok []
