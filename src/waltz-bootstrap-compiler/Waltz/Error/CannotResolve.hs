module Waltz.Error.CannotResolve where

import Waltz.Prelude

newtype Type = Type String

instance Show Type where
    show (Type identifier) = "Cannot resolve '" ++ identifier ++ "'"
