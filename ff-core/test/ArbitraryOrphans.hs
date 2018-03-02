{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryOrphans () where

import           CRDT.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)

import           FF.Types (Note (..), Status (..))

instance Arbitrary Note where
    arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Status where
    arbitrary = arbitraryBoundedEnum
