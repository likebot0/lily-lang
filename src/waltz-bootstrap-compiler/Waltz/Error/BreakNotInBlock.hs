module Waltz.Error.BreakNotInBlock where

import Waltz.Prelude

data Type = Type

instance Show Type where
    show Type = "'break' not in block."
