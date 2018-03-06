{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.FullMap
    ( FullMap (FullMap)
    , Data.FullMap.lookup
    , singleton
    , traverseWithKey
    ) where

import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup)

newtype FullMap key value = FullMap (key -> value)
    deriving (Functor, Monoid, Semigroup)

instance (Bounded key, Enum key, Eq value) => Eq (FullMap key value) where
    (==) = (==) `on` toList

instance (Bounded key, Enum key, Show key, Show value)
        => Show (FullMap key value) where
    show (FullMap f) =
        "\\case "
        ++  intercalate
                "; "
                [ show key ++ " -> " ++ show value
                | key <- keyspace, let value = f key
                ]

instance (Bounded key, Enum key) => Foldable (FullMap key) where
    -- fold :: Monoid m => FullMap key m -> m
    foldMap g = foldMap g . toList
    -- foldr :: (a -> b -> b) -> b -> FullMap key a -> b
    -- foldr' :: (a -> b -> b) -> b -> FullMap key a -> b
    -- foldl :: (b -> a -> b) -> b -> FullMap key a -> b
    -- foldl' :: (b -> a -> b) -> b -> FullMap key a -> b
    -- foldr1 :: (a -> a -> a) -> FullMap key a -> a
    -- foldl1 :: (a -> a -> a) -> FullMap key a -> a
    -- toList :: FullMap key a -> [a]
    toList (FullMap f) = map f keyspace
    -- null :: FullMap key a -> Bool
    -- length :: FullMap key a -> Int
    -- elem :: Eq a => a -> FullMap key a -> Bool
    -- maximum :: Ord a => FullMap key a -> a
    -- minimum :: Ord a => FullMap key a -> a
    -- sum :: Num a => FullMap key a -> a
    -- product :: Num a => FullMap key a -> a

instance (Bounded key, Enum key, Eq key) => Traversable (FullMap key) where
    -- traverse
    --     :: Applicative f => (a -> f b) -> FullMap key a -> f (FullMap key b)
    traverse = traverseWithKey . const
    -- sequenceA :: Applicative f => FullMap key (f a) -> f (FullMap key a)
    -- mapM :: Monad m => (a -> m b) -> FullMap key a -> m (FullMap key b)
    -- sequence :: Monad m => FullMap key (m a) -> m (FullMap key a)

assocs :: (Bounded key, Enum key) => FullMap key value -> [(key, value)]
assocs (FullMap f) = [(key, f key) | key <- keyspace]

fromList :: (Bounded key, Enum key, Eq key) => [value] -> FullMap key value
fromList values = FullMap $ \key ->
    fromMaybe (error "bad key") $ Prelude.lookup key $ zip keyspace values

keyspace :: (Bounded k, Enum k) => [k]
keyspace = [minBound..]

lookup :: key -> FullMap key value -> value
lookup key (FullMap f) = f key

singleton :: Eq key => key -> value -> value -> FullMap key value
singleton key value defaultValue = FullMap $
    \k -> if k == key then value else defaultValue

traverseWithKey
    :: (Applicative t, Bounded k, Enum k, Eq k)
    => (k -> a -> t b) -> FullMap k a -> t (FullMap k b)
traverseWithKey g f = fromList <$> sequenceA [g k a | (k, a) <- assocs f]
