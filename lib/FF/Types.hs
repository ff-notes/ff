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
import           Data.Aeson (FromJSON, ToJSON, Value (Array), camelTo2, object,
                             parseJSON, toJSON, (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import           Data.Aeson.Types (typeMismatch)
import           Data.Foldable (toList)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
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

instance Semigroup Note where
    Note status1 text1 start1 end1 <> Note status2 text2 start2 end2 = Note
        (status1 <> status2) (text1 <> text2) (start1 <> start2) (end1 <> end2)

instance Semilattice Note

deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''Note

instance Collection Note where
    collectionName = "note"

data NoteView = NoteView
    { nid   :: DocId Note
    , text  :: Text
    , start :: Day
    , end   :: Maybe Day
    }
    deriving (Eq, Show)

instance ToJSON NoteView where
    toJSON NoteView{..} = object $
        [Text.pack (show nid) .= text, "start" .= start]
        ++ ["end" .= e | Just e <- pure end]

data Agenda = Agenda
    { notes :: [NoteView]
    , total :: Natural
    }
    deriving (Eq, Show)

noteView :: DocId Note -> Note -> NoteView
noteView nid Note{..} = NoteView
    { nid   = nid
    , text  = LWW.query noteText
    , start = LWW.query noteStart
    , end   = LWW.query noteEnd
    }
