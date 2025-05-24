{-# LANGUAGE RecordWildCards #-}

module Gen (config, contact, day, note) where

import           Prelude hiding (maybe)

import           Data.Char (isControl)
import           Data.Time (Day, fromGregorian)
import           Hedgehog (Gen)
import           Hedgehog.Gen (bool_, choice, enumBounded, integral, maybe,
                               string, text, unicode)
import qualified Hedgehog.Range as Range
import           RON.Data.RGA (RGA (RGA))

import           FF.Config (Config (..), ConfigUI (..))
import           FF.Types (Contact (..), Note (..), NoteStatus (..), Status,
                           Track (..))

path :: Gen FilePath
path = filter (not . isControl) <$> string (Range.linear 1 100) unicode

config :: Gen Config
config = do
    dataDir        <- maybe path
    externalEditor <- maybe path
    ui             <- configUI
    pure Config{..}

configUI :: Gen ConfigUI
configUI = do
    shuffle <- bool_
    pure ConfigUI{..}

day :: Gen Day
day = fromGregorian
    <$> integral (Range.constant 0 10000)
    <*> integral (Range.constant 1 12)
    <*> integral (Range.constant 1 31)

contact :: Gen Contact
contact =
    Contact
    <$> maybe (RGA <$> string (Range.linear 1 100) unicode)
    <*> (Just <$> status)

note :: Gen Note
note = do
    note_end    <- maybe day
    note_start  <- Just <$> day
    note_status <- Just <$> noteStatus
    let note_tags = []
    note_text  <- maybe $ RGA <$> string (Range.linear 1 100) unicode
    note_track <- maybe track
    let note_links = []
    pure Note{..}

noteStatus :: Gen NoteStatus
noteStatus = choice [TaskStatus <$> status, pure Wiki]

track :: Gen Track
track = do
    track_provider   <- Just <$> text (Range.linear 1 100) unicode
    track_source     <- Just <$> text (Range.linear 1 100) unicode
    track_externalId <- Just <$> text (Range.linear 1 100) unicode
    track_url        <- Just <$> text (Range.linear 1 100) unicode
    pure Track{..}

status :: Gen Status
status = enumBounded
