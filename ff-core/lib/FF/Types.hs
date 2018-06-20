{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types where

import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Aeson (camelTo2)
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import           Data.List (genericLength)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           Numeric.Natural (Natural)

import           FF.CrdtAesonInstances ()
import           FF.Storage (Collection, DocId, collectionName)

data Status = Active | Archived | Deleted
    deriving (Bounded, Enum, Eq, Show)

deriveJSON defaultOptions ''Status

data Note = Note
    { noteStatus  :: LWW Status
    , noteText    :: RgaString
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
    { nid    :: NoteId
    , status :: Status
    , text   :: Text
    , start  :: Day
    , end    :: Maybe Day
    }
    deriving (Eq, Show)

data Sample = Sample
    { notes :: [NoteView]
    , total :: Natural
    }
    deriving (Eq, Show)

emptySample :: Sample
emptySample = Sample {notes = [], total = 0}

-- | Number of notes omitted from the sample.
omitted :: Sample -> Natural
omitted Sample { notes, total } = total - genericLength notes

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
    = Overdue   -- ^ end in past
    | EndToday  -- ^ end today
    | EndSoon   -- ^ started, end in future
    | Actual    -- ^ started, no end
    | Starting  -- ^ starting in future
    deriving (Bounded, Enum, Eq, Ord, Show)

taskModes :: [TaskMode]
taskModes = [minBound..]

taskMode :: Day -> NoteView -> TaskMode
taskMode today NoteView { start, end = Nothing } =
    if start <= today then Actual else Starting
taskMode today NoteView { start, end = Just end } = case compare end today of
    LT -> Overdue
    EQ -> EndToday
    GT -> if start <= today then EndSoon else Starting

type ModeMap = Map TaskMode

emptySampleMap :: ModeMap Sample
emptySampleMap = Map.empty

singletonSampleMap :: TaskMode -> Sample -> ModeMap Sample
singletonSampleMap = Map.singleton

singletonTaskModeMap :: Day -> NoteView -> ModeMap [NoteView]
singletonTaskModeMap today note = Map.singleton (taskMode today note) [note]

noteView :: NoteId -> Note -> NoteView
noteView nid Note {..} = NoteView
    { nid    = nid
    , status = LWW.query noteStatus
    , text   = Text.pack $ RGA.toString noteText
    , start  = LWW.query noteStart
    , end    = LWW.query noteEnd
    }

type Limit = Natural
