module Stabel.Parser.Type exposing
    ( FunctionSignature
    , PossiblyQualifiedType(..)
    , PossiblyQualifiedTypeOrStackRange(..)
    )


type PossiblyQualifiedType
    = LocalRef String (List PossiblyQualifiedType)
    | InternalRef (List String) String (List PossiblyQualifiedType)
    | ExternalRef (List String) String (List PossiblyQualifiedType)
    | Generic String
    | FunctionType FunctionSignature


type PossiblyQualifiedTypeOrStackRange
    = StackRange String
    | NotStackRange PossiblyQualifiedType


type alias FunctionSignature =
    { input : List PossiblyQualifiedTypeOrStackRange
    , output : List PossiblyQualifiedTypeOrStackRange
    }
