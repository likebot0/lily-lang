module Waltz.Data.OrderedHashMap where

import Prelude
import Control.Applicative (Const (..))
import Control.Arrow (first, second)
import Data.Hashable (Hashable(hashWithSalt))
import Data.HashMap.Strict (HashMap)
import Data.Semigroup (Semigroup (..))
import qualified Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Sequence
import qualified GHC.Exts as Exts

data OrderedHashMap k v = OrderedHashMap (Data.Sequence.Seq k) (HashMap k v)

instance (Eq k, Eq v) => Eq (OrderedHashMap k v) where
    OrderedHashMap _ a == OrderedHashMap _ b = a == b

instance (Eq k, Hashable k) => Semigroup (OrderedHashMap k v) where
    (<>) = union

instance (Eq k, Hashable k) => Monoid (OrderedHashMap k v) where
    mempty = empty
    mappend = union

instance (Eq k, Hashable k) => Foldable (OrderedHashMap k) where
    foldMap f = foldMap (f . snd) . toList
    null = Prelude.null
    toList = elems
    length = size

instance (Eq k, Hashable k) => Functor (OrderedHashMap k) where
    fmap f (OrderedHashMap xs a) = OrderedHashMap xs $ HashMap.fromList $ Data.Foldable.toList $ (\k -> (k, f $ a HashMap.! k)) <$> xs

instance (Eq k, Hashable k) => Traversable (OrderedHashMap k) where
    traverse f a = traverseWithKey (const f) a

instance (Hashable k, Hashable v) => Hashable (OrderedHashMap k v) where
    hashWithSalt salt (OrderedHashMap _ m) = hashWithSalt salt m

instance (Eq k, Hashable k) => Exts.IsList (OrderedHashMap k v) where
    type Item (OrderedHashMap k v) = (k, v)
    fromList = fromList
    toList = toList

empty :: OrderedHashMap k v
empty = OrderedHashMap Data.Sequence.empty HashMap.empty
{-# INLINE empty #-}

singleton :: Hashable k => k -> v -> OrderedHashMap k v
singleton k v = OrderedHashMap (Data.Sequence.singleton k) (HashMap.singleton k v)
{-# INLINE singleton #-}

null :: OrderedHashMap k v -> Bool
null (OrderedHashMap _ a) = HashMap.null a
{-# INLINE null #-}

size :: OrderedHashMap k v -> Int
size (OrderedHashMap _ a) = HashMap.size a
{-# INLINE size #-}

member :: (Eq k, Hashable k) => k -> OrderedHashMap k a -> Bool
member k (OrderedHashMap _ a) = HashMap.member k a
{-# INLINE member #-}

lookup :: (Eq k, Hashable k) => k -> OrderedHashMap k v -> Maybe v
lookup k (OrderedHashMap _ a) = HashMap.lookup k a
{-# INLINE lookup #-}

lookupDefault :: (Eq k, Hashable k) => v -> k -> OrderedHashMap k v -> v
lookupDefault v k (OrderedHashMap _ a) = HashMap.lookupDefault v k a
{-# INLINE lookupDefault #-}

insert :: (Eq k, Hashable k) => k -> v -> OrderedHashMap k v -> OrderedHashMap k v
insert k v (OrderedHashMap xs a) = do
    case HashMap.lookup k a of
        Nothing -> OrderedHashMap (xs Data.Sequence.|> k) (HashMap.insert k v a)
        Just v  -> OrderedHashMap xs (HashMap.insert k v a)
{-# INLINE insert #-}

unionWith
    :: (Eq k, Hashable k)
    => (v -> v -> v)
    -> OrderedHashMap k v -> OrderedHashMap k v -> OrderedHashMap k v
unionWith f (OrderedHashMap i a) (OrderedHashMap j b) = OrderedHashMap i $ HashMap.unionWith f a b

unionWithKey
    :: (Eq k, Hashable k)
    => (k -> v -> v -> v)
    -> OrderedHashMap k v -> OrderedHashMap k v -> OrderedHashMap k v
unionWithKey f (OrderedHashMap i a) (OrderedHashMap j b) = OrderedHashMap i $ HashMap.unionWithKey f a b

union
    :: (Eq k, Hashable k)
    => OrderedHashMap k v -> OrderedHashMap k v -> OrderedHashMap k v
union = unionWith const

unions
    :: (Eq k, Hashable k, Foldable f)
    => f (OrderedHashMap k v) -> OrderedHashMap k v
unions = Data.Foldable.foldl' union empty

mapKeys :: (Eq k', Hashable k') => (k -> k') -> OrderedHashMap k v -> OrderedHashMap k' v
mapKeys f (OrderedHashMap ks a) =
    OrderedHashMap
        do f <$> ks
        do HashMap.mapKeys f a

map :: (Eq k, Hashable k) => (v -> v') -> OrderedHashMap k v -> OrderedHashMap k v'
map = fmap

mapWithKey :: (k -> v -> v') -> OrderedHashMap k v -> OrderedHashMap k v'
mapWithKey f (OrderedHashMap xs a) = OrderedHashMap xs $ HashMap.mapWithKey f a

foldMapWithKey :: (Eq k, Hashable k, Monoid m) => (k -> v -> m) -> OrderedHashMap k v -> m
foldMapWithKey f = foldMap (uncurry f) . toList

traverseWithKey :: Applicative f => (k -> a -> f b) -> OrderedHashMap k a -> f (OrderedHashMap k b)
traverseWithKey f (OrderedHashMap xs a) = OrderedHashMap xs <$> HashMap.traverseWithKey f a

difference
    :: (Eq k, Hashable k)
    => OrderedHashMap k v -> OrderedHashMap k w -> OrderedHashMap k v
difference (OrderedHashMap xs a) (OrderedHashMap _ b) = do
    let c = HashMap.difference a b

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` c) xs) c

intersection
    :: (Eq k, Hashable k)
    => OrderedHashMap k v -> OrderedHashMap k w -> OrderedHashMap k v
intersection = intersectionWith const

intersectionWith
    :: (Eq k, Hashable k)
    => (v1 -> v2 -> v3)
    -> OrderedHashMap k v1 -> OrderedHashMap k v2 -> OrderedHashMap k v3
intersectionWith f = intersectionWithKey (const f)

intersectionWithKey
    :: (Eq k, Hashable k)
    => (k -> v1 -> v2 -> v3)
    -> OrderedHashMap k v1 -> OrderedHashMap k v2 -> OrderedHashMap k v3
intersectionWithKey f (OrderedHashMap xs a) (OrderedHashMap _ b) = do
    let c = HashMap.intersectionWithKey f a b

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` c) xs) c

foldl' :: (Eq k, Hashable k) => (a -> v -> a) -> a -> OrderedHashMap k v -> a
foldl' f a = Data.Foldable.foldl' f a . elems
{-# INLINE foldl' #-}

foldlWithKey' :: (Eq k, Hashable k) => (a -> k -> v -> a) -> a -> OrderedHashMap k v -> a
foldlWithKey' f a = Data.Foldable.foldl' (\a (k, v) -> f a k v) a . toList
{-# INLINE foldlWithKey' #-}

foldr :: (Eq k, Hashable k) => (v -> a -> a) -> a -> OrderedHashMap k v -> a
foldr f a = Data.Foldable.foldr f a . elems
{-# INLINE foldr #-}

foldrWithKey :: (Eq k, Hashable k) => (k -> v -> a -> a) -> a -> OrderedHashMap k v -> a
foldrWithKey f a = Data.Foldable.foldr (\(k, v) a -> f k v a) a . toList
{-# INLINE foldrWithKey #-}

filter :: (Eq k, Hashable k) => (v -> Bool) -> OrderedHashMap k v -> OrderedHashMap k v
filter f (OrderedHashMap xs a) = do
    let b = HashMap.filter f a

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` b) xs) b

filterWithKey :: (Eq k, Hashable k) => (k -> v -> Bool) -> OrderedHashMap k v -> OrderedHashMap k v
filterWithKey f (OrderedHashMap xs a) = do
    let b = HashMap.filterWithKey f a

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` b) xs) b

mapMaybe :: (Eq k, Hashable k) => (v -> Maybe v') -> OrderedHashMap k v -> OrderedHashMap k v'
mapMaybe f (OrderedHashMap xs a) = do
    let b = HashMap.mapMaybe f a

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` b) xs) b

mapMaybeWithKey :: (Eq k, Hashable k) => (k -> v -> Maybe v') -> OrderedHashMap k v -> OrderedHashMap k v'
mapMaybeWithKey f (OrderedHashMap xs a) = do
    let b = HashMap.mapMaybeWithKey f a

    OrderedHashMap (Data.Sequence.filter (`HashMap.member` b) xs) b

keys :: (Eq k, Hashable k) => OrderedHashMap k v -> [k]
keys (OrderedHashMap xs _) = Data.Foldable.toList xs 
{-# INLINE keys #-}

elems :: (Eq k, Hashable k) => OrderedHashMap k v -> [v]
elems (OrderedHashMap xs a) = Data.Foldable.toList $ (a HashMap.!) <$> xs
{-# INLINE elems #-}

fromList :: (Eq k, Hashable k) => [(k, v)] -> OrderedHashMap k v
fromList a = OrderedHashMap (Data.Sequence.fromList $ fst <$> a) (HashMap.fromList a)
{-# INLINE fromList #-}

toList :: (Eq k, Hashable k) => OrderedHashMap k v -> [(k, v)]
toList (OrderedHashMap xs a) = Data.Foldable.toList $ (\k -> (k, a HashMap.! k)) <$> xs
{-# INLINE toList #-}
