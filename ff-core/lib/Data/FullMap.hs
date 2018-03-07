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
    show f =
        "\\case "
        ++  intercalate
                "; "
                [show key ++ " -> " ++ show value | (key, value) <- assocs f]

instance (Bounded key, Enum key) => Foldable (FullMap key) where
    foldMap g = foldMap g . toList
    toList (FullMap f) = map f keyspace

instance (Bounded key, Enum key, Eq key) => Traversable (FullMap key) where
    traverse = traverseWithKey . const

assocs :: (Bounded key, Enum key) => FullMap key value -> [(key, value)]
assocs (FullMap f) = [ (key, f key) | key <- keyspace ]

fromList :: (Bounded key, Enum key, Eq key) => [value] -> FullMap key value
fromList values = FullMap $ \key ->
    fromMaybe (error "bad key") $ Prelude.lookup key $ zip keyspace values

keyspace :: (Bounded k, Enum k) => [k]
keyspace = [minBound ..]

lookup :: key -> FullMap key value -> value
lookup key (FullMap f) = f key

singleton :: Eq key => key -> value -> value -> FullMap key value
singleton key value defaultValue =
    FullMap $ \k -> if k == key then value else defaultValue

traverseWithKey
    :: (Applicative t, Bounded k, Enum k, Eq k)
    => (k -> a -> t b)
    -> FullMap k a
    -> t (FullMap k b)
traverseWithKey g f = fromList <$> sequenceA [ g k a | (k, a) <- assocs f ]
