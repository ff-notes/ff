{-# LANGUAGE RecordWildCards #-}

module Gen (config, contact, day, note) where

import           Prelude hiding (maybe)

import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, fromGregorian)
import           Hedgehog (Gen)
import           Hedgehog.Gen (bool_, choice, enumBounded, integral, list,
                               maybe, string, text, unicode)
import qualified Hedgehog.Range as Range
import           RON.Data.RGA (RGA (RGA))
import           RON.Types (ObjectRef (ObjectRef), UUID)
import qualified RON.UUID as UUID

import           FF.Config (Config (..), ConfigUI (..))
import           FF.Types (Contact (..), Note (..), NoteStatus (..), Status,
                           Track (..))

config :: Gen Config
config = do
    dataDir <- maybe $ string (Range.linear 1 100) unicode
    ui <- configUI
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
note = Note
    <$> maybe day
    <*> (Just <$> day)
    <*> (Just <$> noteStatus)
    <*> tags
    <*> maybe (RGA <$> string (Range.linear 1 100) unicode)
    <*> maybe track
  where
    tags = do
        mUid <- uid
        case mUid of
            Nothing -> pure []
            Just uuid -> list (Range.linear 0 10) (pure $ ObjectRef uuid)

noteStatus :: Gen NoteStatus
noteStatus = choice [TaskStatus <$> status, pure Wiki]

track :: Gen Track
track = Track
    <$> (Just <$> text (Range.linear 1 100) unicode)
    <*> (Just <$> text (Range.linear 1 100) unicode)
    <*> (Just <$> text (Range.linear 1 100) unicode)
    <*> (Just <$> text (Range.linear 1 100) unicode)

status :: Gen Status
status = enumBounded

tag :: Gen Text
tag = text (Range.linear 1 100) unicode

uid :: Gen (Maybe UUID)
uid = UUID.mkName . encodeUtf8 <$> tag
