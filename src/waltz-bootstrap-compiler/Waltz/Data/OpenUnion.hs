module Waltz.Data.OpenUnion
    ( Union()
    , (@>)
    , inject
    , is
    , isNot
    , typesExhausted
    )
where

import Data.Type.Bool
import Data.Word
import GHC.TypeLits
import Prelude
import Unsafe.Coerce
import Waltz.Data.Proxy
import qualified Waltz.Data.Instance
import qualified Waltz.Data.Type.List

data Union (s :: [*]) = forall a. Union (Index s) a

type family Index (s :: [*]) :: * where
    Index s =
        If (Waltz.Data.Type.List.Length s <=? 2 ^ 8) Word8
        ( If (Waltz.Data.Type.List.Length s <=? 2 ^ 16) Word16
          ( If (Waltz.Data.Type.List.Length s <=? 2 ^ 32) Word32
            ( If (Waltz.Data.Type.List.Length s <=? 2 ^ 64) Word64 Integer
        ) ) )

instance Waltz.Data.Instance.ToInstance c (Union '[]) where
    toInstance = typesExhausted

instance (c a, Constraints (a : as) a, Waltz.Data.Instance.ToInstance c (Union (Waltz.Data.Type.List.Delete a as))) => Waltz.Data.Instance.ToInstance c (Union (a : as)) where
    toInstance x = case is @a x of
        Left x -> Waltz.Data.Instance.toInstance x
        Right x -> Waltz.Data.Instance.Instance x

instance Eq (Union '[]) where
    (==) x _ = typesExhausted x

instance (Eq a, Constraints (a : as) a, Eq (Union (Waltz.Data.Type.List.Delete a as))) => Eq (Union (a : as)) where
    (==) x y =
        case (is @a x, is @a y) of
            (Left x, Left y) -> x == y
            (Right x, Right y) -> y == y
            _ -> False


type Constraints (s :: [*]) (a :: *) =
    ( IndexConstraints s
    , IndexConstraints (Waltz.Data.Type.List.Delete a s)
    , KnownNat (Waltz.Data.Type.List.Index s a)
    )

type IndexConstraints (s :: [*]) =
    ( Eq (Index s)
    , Ord (Index s)
    , Num (Index s)
    , Integral (Index s)
    )

index :: forall s a. Constraints s a => Index s
index = fromInteger $ natVal $ Proxy @(Waltz.Data.Type.List.Index s a)
{-# INLINE index #-}

inject :: forall s a. Constraints s a => Waltz.Data.Type.List.Elem a s => a -> Union s
inject = Union $ index @s @a
{-# INLINE inject #-}

is :: forall a s. Constraints s a => Union s -> Either (Union (Waltz.Data.Type.List.Delete a s)) a 
is (Union i x) = 
    if i /= index @s @a
        then Left $ Union @(Waltz.Data.Type.List.Delete a s) (fromIntegral (if i < (index @s @a) then i else i - 1)) x
        else Right $ unsafeCoerce x
{-# INLINE is #-}

isNot :: forall a s. Constraints s a => Union s -> Either a (Union (Waltz.Data.Type.List.Delete a s)) 
isNot (Union i x) = 
    if i /= index @s @a
        then Right $ Union @(Waltz.Data.Type.List.Delete a s) (fromIntegral (if i < (index @s @a) then i else i - 1)) x
        else Left $ unsafeCoerce x
{-# INLINE isNot #-}

typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}

(@>) :: forall a s b. Constraints s a => (a -> b) -> (Union (Waltz.Data.Type.List.Delete a s) -> b) -> Union s -> b
r @> l = either l r . is
{-# INLINE (@>) #-}

infixr 2 @>
