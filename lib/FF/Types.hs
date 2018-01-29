{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types where

import           CRDT.LamportClock (LamportTime (LamportTime), Pid)
import           CRDT.LWW (LWW (LWW), time, value)
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, ToJSON, Value (Array), camelTo2,
                             parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import           Data.Aeson.Types (typeMismatch)
import           Data.Foldable (toList)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import           Data.Time (Day)
import           GHC.Exts (fromList)
import           Numeric.Natural (Natural)

import           FF.Storage (Collection, DocId, collectionName)

deriveJSON defaultOptions ''Pid

instance FromJSON a => FromJSON (LWW a) where
    parseJSON (Array a) = case toList a of
        [valueJ, timeJ, pidJ] -> LWW <$> parseJSON valueJ <*> parseTime
          where
            parseTime = LamportTime <$> parseJSON timeJ <*> parseJSON pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]
    parseJSON v = typeMismatch "Array" v

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time pid} =
        Array $ fromList [toJSON value, toJSON time, toJSON pid]

data Status = Active | Archived | Deleted
    deriving (Eq, Show)

deriveJSON defaultOptions ''Status

data Note = Note
    { noteStatus  :: LWW Status
    , noteText    :: LWW Text
    , noteStart   :: LWW Day
    , noteEnd     :: LWW (Maybe Day)
    }
    deriving (Eq, Show)

type NoteId = DocId Note

instance Semigroup Note where
    Note status1 text1 start1 end1 <> Note status2 text2 start2 end2 = Note
        (status1 <> status2) (text1 <> text2) (start1 <> start2) (end1 <> end2)

instance Semilattice Note

deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''Note

instance Collection Note where
    collectionName = "note"

data NoteView = NoteView
    { nid   :: NoteId
    , text  :: Text
    , start :: Day
    , end   :: Maybe Day
    }
    deriving (Eq, Show)

data Sample = Sample
    { notes :: [NoteView]
    , total :: Natural
    }
    deriving (Eq, Show)

emptySample :: Sample
emptySample = Sample{notes = [], total = 0}

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
    = Overdue   -- ^ end in past
    | EndToday  -- ^ end today
    | EndSoon   -- ^ started, end in future
    | Actual    -- ^ started, no end
    | Starting  -- ^ starting in future
    deriving (Eq, Ord, Show)

taskMode :: Day -> NoteView -> TaskMode
taskMode today NoteView{start, end = Nothing} =
    if start <= today then Actual else Starting
taskMode today NoteView{start, end = Just end} =
    case compare end today of
        LT -> Overdue
        EQ -> EndToday
        GT -> if start <= today then EndSoon else Starting

data ModeMap a = ModeMap
    { overdue  :: a
    , endToday :: a
    , endSoon  :: a
    , actual   :: a
    , starting :: a
    }
    deriving (Eq, Show)

instance Semigroup a => Semigroup (ModeMap a) where
    ModeMap o1 t1 e1 a1 s1 <> ModeMap o2 t2 e2 a2 s2 =
        ModeMap (o1 <> o2) (t1 <> t2) (e1 <> e2) (a1 <> a2) (s1 <> s2)

instance (Semigroup a, Monoid a) => Monoid (ModeMap a) where
    mempty = ModeMap
        { overdue  = mempty
        , endToday = mempty
        , endSoon  = mempty
        , actual   = mempty
        , starting = mempty
        }
    mappend = (<>)

emptySampleMap :: ModeMap Sample
emptySampleMap = ModeMap
    { overdue  = emptySample
    , endToday = emptySample
    , endSoon  = emptySample
    , actual   = emptySample
    , starting = emptySample
    }

singletonModeMap :: (Semigroup a, Monoid a) => TaskMode -> a -> ModeMap a
singletonModeMap Overdue  a = mempty{overdue  = a}
singletonModeMap EndToday a = mempty{endToday = a}
singletonModeMap EndSoon  a = mempty{endSoon  = a}
singletonModeMap Actual   a = mempty{actual   = a}
singletonModeMap Starting a = mempty{starting = a}

singletonTaskModeMap :: Day -> NoteView -> ModeMap [NoteView]
singletonTaskModeMap today note = singletonModeMap (taskMode today note) [note]

noteView :: NoteId -> Note -> NoteView
noteView nid Note{..} = NoteView
    { nid   = nid
    , text  = LWW.query noteText
    , start = LWW.query noteStart
    , end   = LWW.query noteEnd
    }
