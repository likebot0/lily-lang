module Waltz.Context.ResolveLocal.Define where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Error.CannotResolve

instance Define "waltz-bootstrap-compiler/context/resolve-local" where
    call' = fun \identifier -> do
        raise $ Waltz.Error.CannotResolve.Type identifier
