module Waltz.Config.GetEntrypoints.Define where

import Waltz.Prelude
import Waltz.Declare
import qualified System.Environment

instance Define "waltz-bootstrap-compiler/config/get-entrypoints" where
    call' = fun \_ -> do
        io System.Environment.getArgs
