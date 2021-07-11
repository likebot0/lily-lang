module Waltz.Data.Proxy where

import Prelude
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data Proxy a = Proxy

instance KnownSymbol a => Show (Proxy a) where
    show = symbolVal
