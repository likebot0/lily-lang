module Waltz.Ast.Syntax where

import Data.Aeson
import qualified Waltz.Ast
import qualified Waltz.Diagnostic

type instance Waltz.Ast.Attributes "syntax-analyzed" a = Attributes

data Attributes =
    Attributes
        { location :: Waltz.Diagnostic.Location
        }

instance Data.Aeson.ToJSON Attributes where
    toJSON (Attributes location) = object
        [ "location" .= location
        ]
