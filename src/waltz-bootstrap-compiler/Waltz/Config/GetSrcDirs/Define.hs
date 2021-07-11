module Waltz.Config.GetSrcDirs.Define where

import Waltz.Prelude
import Waltz.Declare

instance Define "waltz-bootstrap-compiler/config/get-src-dirs" where
    call' = fun \_ -> do
        return ["./src"]
