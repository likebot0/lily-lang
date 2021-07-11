module Waltz.Data.Type.List where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Prelude

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    '[] ++ bs = bs
    (a : as) ++ bs = a : (as ++ bs)
infixr 5 ++

type family Elem (a :: k) (b :: [k]) :: Constraint where
    Elem a (a : bs) = ()
    Elem a (b : bs) = (Elem a bs)

type family SubList (a :: [k]) (b :: [k]) :: Constraint where
    SubList '[] bs = ()
    SubList (a : as) bs = (Elem a bs, SubList as bs)

type family Delete (a :: k) (s :: [k]) :: [k] where
    Delete a '[] = '[]
    Delete a (a : as) = Delete a as
    Delete a (b : as) = b : Delete a as

type family Index (as :: [*]) (a :: *) :: Nat where
    Index (a : as) a = 0
    Index (b : as) a = 1 + Index as a

type family Length (as :: [*]) :: Nat where
    Length '[] = 0
    Length (a : as) = 1 + Length as

index :: forall s a. Proxy (Index s a)
index = Proxy @(Index s a)
{-# INLINE index #-}
