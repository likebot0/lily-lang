module Waltz.Ast.Semantic where

import Waltz.Prelude
import Data.Aeson
import qualified Waltz.Ast
import qualified Waltz.Data.Map
import qualified Waltz.Diagnostic

type instance Waltz.Ast.Attributes "semantic-analyzed" "block-expression" =
    AttributesWithType "unknown"

type instance Waltz.Ast.Attributes "semantic-analyzed" "discard" =
    Waltz.Ast.Attributes "syntax-analyzed" "discard"

type instance Waltz.Ast.Attributes "semantic-analyzed" "expression" =
    AttributesWithType "expression"

type instance Waltz.Ast.Attributes "semantic-analyzed" "grouped-expression" =
    AttributesWithType "expression"

type instance Waltz.Ast.Attributes "semantic-analyzed" "identifier" =
    AttributesWithType "expression"

type instance Waltz.Ast.Attributes "semantic-analyzed" "key-value" =
    Waltz.Ast.Attributes "syntax-analyzed" "key-value"

type instance Waltz.Ast.Attributes "semantic-analyzed" "lambda-expression" =
    AttributesWithType "unknown"

type instance Waltz.Ast.Attributes "semantic-analyzed" "literal/array" =
    AttributesWithType "literal/array"

type instance Waltz.Ast.Attributes "semantic-analyzed" "literal/number" =
    AttributesWithType "literal/number"

type instance Waltz.Ast.Attributes "semantic-analyzed" "literal/object" =
    AttributesWithType "literal/object"

type instance Waltz.Ast.Attributes "semantic-analyzed" "literal/string" =
    AttributesWithType "literal/string"

type instance Waltz.Ast.Attributes "semantic-analyzed" "root" =
    Waltz.Ast.Attributes "syntax-analyzed" "root"

type instance Waltz.Ast.Attributes "semantic-analyzed" "type-expression" =
    AttributesWithType "unknown"

type instance Waltz.Ast.Attributes "semantic-analyzed" "statement/base" =
    Waltz.Ast.Attributes "syntax-analyzed" "statement/base"

type instance Waltz.Ast.Attributes "semantic-analyzed" "statement/include" =
    Waltz.Ast.Attributes "syntax-analyzed" "statement/include"

type instance Waltz.Ast.Attributes "semantic-analyzed" "statement/if" =
    Waltz.Ast.Attributes "syntax-analyzed" "statement/if"

type instance Waltz.Ast.Attributes "semantic-analyzed" "statement/let" =
    Waltz.Ast.Attributes "syntax-analyzed" "statement/let"

type instance Waltz.Ast.Attributes "semantic-analyzed" "statement/with" =
    Waltz.Ast.Attributes "syntax-analyzed" "statement/with"

newtype Type (s :: Symbol) = Type (Data s)

type family Data (s :: Symbol) where
    Data "expression" =
        Union
            [ Type "lambda-expression"
            , Type "literal/array"
            , Type "literal/number"
            , Type "literal/object"
            , Type "literal/string"
            , Type "unknown"
            , Type "invalud"
            ]

    Data "lambda-expression" =
        ()

    Data "literal/array" =
        [Type "expression"]

    Data "literal/number" =
        String

    Data "literal/object" =
        Waltz.Data.Map.Map String (Type "expression")

    Data "literal/string" = 
        String

    Data "invalud" =
        ()

    Data "unknown" =
        ()

data AttributesWithType s =
    AttributesWithType
        Waltz.Diagnostic.Location
        (Type s)

instance Data.Aeson.ToJSON (AttributesWithType s) where
    toJSON (AttributesWithType location _) = object
        [ "location" .= location
        ]
