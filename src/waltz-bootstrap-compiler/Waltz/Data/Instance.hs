module Waltz.Data.Instance where

import Prelude
import Data.Kind (Constraint)

data Instance (c :: * -> Constraint) = forall a. c a => Instance a

dispatch :: forall (c :: * -> Constraint) (b :: *). (forall a. c a => a -> b) -> Instance c -> b
dispatch f (Instance x) = f x
{-# INLINE dispatch #-}

class ToInstance (c :: * -> Constraint) (a :: *) where
    toInstance :: a -> Instance c
