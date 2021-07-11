module Waltz.Error.InternalCompilerError where

import Waltz.Prelude

newtype Type = Type String

instance Show Type where
    show (Type message) = "Internal compiler error: " ++ message
