module Waltz.Error.ReturnNotInFunction where

import Waltz.Prelude

data Type = Type

instance Show Type where
    show Type = "'return' not in function."
