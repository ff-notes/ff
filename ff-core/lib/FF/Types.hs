{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FF.Types where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (ask, runReaderT)
import qualified Data.Aeson as JSON
import           Data.Aeson.Extra (ToJSON, singletonObjectSum, toJSON, (.=))
import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as BSL
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.List (genericLength)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Time (diffDays)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           RON.Data (MonadObjectState, Replicated (encoding),
                           ReplicatedAsPayload (fromPayload, toPayload),
                           evalObjectState, payloadEncoding, readObject)
import           RON.Data.RGA (RGA)
import           RON.Data.Time (Day)
import           RON.Error (Error (Error), MonadE)
import           RON.Schema.TH (mkReplicated)
import           RON.Storage (Collection, DocId, collectionName, loadDocument)
import           RON.Storage.Backend (DocId (DocId),
                                      Document (Document, objectFrame),
                                      MonadStorage)
import           RON.Text.Serialize (serializeUuid)
import           RON.Types (Atom (AUuid), ObjectRef (ObjectRef), UUID)
import qualified RON.UUID as UUID

instance ToJSON UUID where
  toJSON = JSON.String . uuidToText

data NoteStatus = TaskStatus Status | Wiki
  deriving (Eq, Show)

wiki :: UUID
wiki = fromJust $ UUID.mkName "Wiki"

instance Replicated NoteStatus where
  encoding = payloadEncoding

instance ReplicatedAsPayload NoteStatus where

  toPayload = \case
    TaskStatus status -> toPayload status
    Wiki -> toPayload wiki

  fromPayload = \case
    [AUuid u] | u == wiki -> pure Wiki
    p -> TaskStatus <$> fromPayload p

[mkReplicated|
  (enum Status
    Active Archived
  )

  (opaque_atoms NoteStatus)
    ; TODO(2018-12-05, https://github.com/ff-notes/ron/issues/115, cblp)
    ; (enum NoteStatus (extends Status) Wiki)

  (struct_set Contact
    #haskell {field_prefix "contact_"}
    (status lww   Status)
    (name   merge RgaString)
  )

  ; TODO(2019-08-08, #163, cblp) remove a year after release of Track(3)
  ; ff 0.12 is released on 2019-08-14
  (struct_lww TrackV2
    #haskell {field_prefix "trackV2_"}
    (provider   String)
    (source     String)
    (externalId String)
    (url        String)
  )

  (struct_set Track
    #haskell {field_prefix "track_"}
    (provider   lww String)
    (source     lww String)
    (externalId lww String)
    (url        lww String)
  )

  (struct_set Tag
    #haskell {field_prefix "tag_"}
    (text lww String)
  )

  ; TODO(2019-08-08, #163, cblp) remove a year after release of Note(3)
  ; ff 0.12 is released on 2019-08-14
  (struct_lww NoteV2
    #haskell {field_prefix "noteV2_"}
    (status NoteStatus)
    (text   RgaString)
    (start  Day)
    (end    Day)
    (track  TrackV2)
  )

  (struct_set Note
    #haskell {field_prefix "note_"}
    (status lww   NoteStatus)
    (text   merge RgaString)
    (start  lww   Day)
    (end    lww   Day)
    (tags   set   (ObjectRef Tag))
    (track  merge Track)
    (links  set   (ObjectRef Link))
  )

  (enum LinkType
    SubNote ; a note (target) is a part of another note (source),
            ; e. g. a subtask
    )

  (struct_set Link
    #haskell {field_prefix "link_"}
    (target lww (ObjectRef Note))
    (type   lww LinkType)
  )
|]

deriving instance Eq Contact

deriving instance Show Contact

deriving instance Eq Note

deriving instance Show Note

deriving instance Bounded Status

deriving instance Enum Status

deriving instance Eq Status

deriving instance Show Status

deriving instance Eq Track

deriving instance Generic Track

deriving instance Hashable Track

deriving instance Show Track

deriving instance Eq Tag

deriving instance Show Tag

type NoteId = DocId Note

type ContactId = DocId Contact

type TagId = DocId Tag

instance Collection Note where
  collectionName = "note"

instance Collection Contact where
  collectionName = "contact"

instance Collection Tag where
  collectionName = "tag"

data Sample a = Sample{items :: [a], total :: Natural}
  deriving (Eq, Functor, Show)

instance ToJSON a => ToJSON (Sample a) where
  toJSON Sample{items} = toJSON items

{- |
  A value identified with some document.
  Should not be used directly, use 'EntityDoc' or 'EntityView' instead.
-}
data Entity doc val
  = Collection doc => Entity{entityId :: DocId doc, entityVal :: val}

deriving instance Eq val => Eq (Entity doc val)

deriving instance Show val => Show (Entity doc val)

instance ToJSON val => ToJSON (Entity doc val) where
  toJSON e = JSON.object [entityToJson e]
  toJSONList = JSON.object . map entityToJson

entityToJson :: ToJSON val => Entity doc val -> JSON.Pair
entityToJson Entity{entityId = DocId entityId, entityVal} = key .= entityVal
  where
    key =
      maybe
        (error "entityId is not a valid RON-UUID")
        (Text.decodeUtf8 . BSL.toStrict . serializeUuid)
        (UUID.decodeBase32 entityId)

type EntityDoc doc = Entity doc doc

type EntityView doc = Entity doc (View doc)

type ContactSample = Sample (EntityDoc Contact)

type NoteSample = Sample (EntityView Note)

emptySample :: Sample a
emptySample = Sample{items = [], total = 0}

-- | Number of notes omitted from the sample.
omitted :: Sample a -> Natural
omitted Sample{total, items} = total - genericLength items

data family View doc

data instance View Note = NoteView
  { note :: Note
  , tags :: HashMap Text Text -- ^ the key is UUID or URI of the tag
  }
  deriving (Eq, Show)

instance ToJSON (View Note) where
  toJSON NoteView{note, tags} =
    JSON.Object $ HashMap.insert "note_tags" tags' noteObj
    where
      noteObj = case toJSON note of
        JSON.Object obj -> obj
        _               -> error "Note must be serialized to Object"
      tags' = JSON.Object $ JSON.String <$> tags

uuidToText :: UUID -> Text
uuidToText = Text.decodeUtf8 . BSL.toStrict . serializeUuid

type ModeMap = Map TaskMode

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
  = Overdue Natural -- ^ end in past, with days
  | EndToday -- ^ end today
  | EndSoon Natural -- ^ started, end in future, with days
  | Actual -- ^ started, no end
  | Starting Natural -- ^ starting in future, with days
  deriving (Eq, Show)

taskModeOrder :: TaskMode -> Int
taskModeOrder = \case
  Overdue  _ -> 0
  EndToday   -> 1
  EndSoon  _ -> 2
  Actual     -> 3
  Starting _ -> 4

instance Ord TaskMode where
  Overdue  n <= Overdue  m = n               >= m
  EndSoon  n <= EndSoon  m = n               <= m
  Starting n <= Starting m = n               <= m
  n          <= m          = taskModeOrder n <= taskModeOrder m

taskMode :: Day -> Note -> TaskMode
taskMode today Note{note_start, note_end} =
  case note_end of
    Nothing
      | start <= today -> Actual
      | otherwise      -> starting start today
    Just e -> case compare e today of
      LT -> overdue today e
      EQ -> EndToday
      GT
        | start <= today -> endSoon e today
        | otherwise      -> starting start today
  where
    start = fromJust note_start
    overdue = helper Overdue
    endSoon = helper EndSoon
    starting = helper Starting
    helper m x y = m . fromIntegral $ diffDays x y

type Limit = Natural

-- * Legacy, v2
loadNote :: MonadStorage m => NoteId -> m (EntityDoc Note)
loadNote entityId = do
  Document{objectFrame} <- loadDocument entityId
  let tryCurrentEncoding = evalObjectState objectFrame readObject
  case tryCurrentEncoding of
    Right entityVal -> pure Entity{entityId, entityVal}
    Left e1 -> do
      let tryNote2Encoding = evalObjectState objectFrame readNoteFromV2
      case tryNote2Encoding of
        Right entityVal -> pure Entity{entityId, entityVal}
        Left e2 -> throwError $ Error "loadNote" [e1, e2]

readNoteFromV2 :: (MonadE m, MonadObjectState a m) => m Note
readNoteFromV2 = do
  ObjectRef uuid <- ask
  NoteV2{..} <- runReaderT readObject (ObjectRef @NoteV2 uuid)
  pure Note
    { note_end = noteV2_end,
      note_start = noteV2_start,
      note_status = noteV2_status,
      note_text = noteV2_text,
      note_tags = [],
      note_track = trackFromV2 <$> noteV2_track,
      note_links = []
    }

trackFromV2 :: TrackV2 -> Track
trackFromV2 TrackV2{..} =
  Track
    { track_externalId = trackV2_externalId,
      track_provider = trackV2_provider,
      track_source = trackV2_source,
      track_url = trackV2_url
    }

deriveToJSON defaultOptions     ''Contact
deriveToJSON defaultOptions     ''Link
deriveToJSON defaultOptions     ''LinkType
deriveToJSON defaultOptions     ''Note
deriveToJSON singletonObjectSum ''NoteStatus
deriveToJSON defaultOptions     ''ObjectRef
deriveToJSON defaultOptions     ''RGA
deriveToJSON defaultOptions     ''Status
deriveToJSON defaultOptions     ''Tag
deriveToJSON defaultOptions     ''TaskMode
deriveToJSON defaultOptions     ''Track
