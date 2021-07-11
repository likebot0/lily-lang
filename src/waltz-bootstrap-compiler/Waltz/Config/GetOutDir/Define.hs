module Waltz.Config.GetOutDir.Define where

import Waltz.Prelude
import Waltz.Declare

instance Define "waltz-bootstrap-compiler/config/get-out-dir" where
    call' = fun \_ -> do
        return "./out"
