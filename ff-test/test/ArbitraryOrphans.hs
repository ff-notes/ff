{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryOrphans
    ()
where

import           CRDT.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)

import           FF.Config (Config (..), ConfigUI (..))
import           FF.Types (Note (..), NoteStatus (..), Status (..),
                           Tracked (..))

instance Arbitrary Config where
    arbitrary = Config <$> arbitrary <*> arbitrary

instance Arbitrary ConfigUI where
    arbitrary = ConfigUI <$> arbitrary

instance Arbitrary Note where
    arbitrary = Note
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NoteStatus where
    arbitrary = TaskStatus <$> arbitrary

instance Arbitrary Status where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Tracked where
    arbitrary = Tracked <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
