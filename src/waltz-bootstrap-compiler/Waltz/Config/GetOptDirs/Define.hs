module Waltz.Config.GetOptDirs.Define where

import Waltz.Prelude
import Waltz.Declare

instance Define "waltz-bootstrap-compiler/config/get-opt-dirs" where
    call' = fun \_ -> do
        return ["./opt"]
