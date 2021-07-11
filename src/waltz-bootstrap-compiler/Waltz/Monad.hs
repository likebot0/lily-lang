module Waltz.Monad where

import GHC.TypeLits
import Waltz.Data.OpenUnion
import Waltz.Data.Type.List

import Control.Monad ((>=>))
import Prelude

-- type Eff (effs :: [ * -> * ]) = Freer (Union effs)

-- data Freer t a = Pure a | forall x . Bind (t x) (x -> Freer t a)

-- instance Functor (Freer f) where
--     f `fmap` Pure x = Pure $ f x
--     f `fmap` Bind tx k = tx `Bind` (k >=> Pure . f)

-- instance Applicative (Freer f) where
--     pure = Pure
--     Pure f <*> m = f <$> m
--     tx `Bind` q <*> m = tx `Bind` (q >=> (<$> m))

-- instance Monad (Freer f) where
--     Pure x >>= f = f x
--     tx `Bind` k >>= f = tx `Bind` (k >=> f)
