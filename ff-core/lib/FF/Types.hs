{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FF.Types where

import qualified CRDT.Cv.RGA as CRDT
import qualified CRDT.LWW as CRDT
import qualified CRDT.LamportClock as CRDT
import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, runReaderT)
import Data.Aeson ((.:), (.:?), FromJSON, eitherDecode, parseJSON, withObject)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy (ByteString)
import Data.Hashable (Hashable)
import Data.List (genericLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (diffDays)
import FF.CrdtAesonInstances ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import RON.Data
  ( MonadObjectState,
    Replicated (encoding),
    ReplicatedAsPayload (fromPayload, toPayload),
    evalObjectState,
    payloadEncoding,
    readObject,
    stateFromChunk,
    stateToWireChunk,
  )
import RON.Data.LWW (lwwType)
import RON.Data.RGA (RgaRep)
import RON.Data.Time (Day)
import RON.Epoch (localEpochTimeFromUnix)
import RON.Error (Error (Error), MonadE, liftEitherString)
import RON.Event (Event (Event), applicationSpecific, encodeEvent)
import RON.Schema.TH (mkReplicated)
import RON.Storage
  ( Collection,
    DocId,
    collectionName,
    fallbackParse,
    loadDocument,
  )
import RON.Storage.Backend (Document (Document, objectFrame), MonadStorage)
import RON.Types
  ( Atom (AUuid),
    ObjectFrame (ObjectFrame, frame, uuid),
    ObjectRef (ObjectRef),
    Op (Op),
    UUID,
    WireStateChunk (WireStateChunk, stateBody, stateType),
  )
import qualified RON.UUID as UUID

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
    Active Archived)

  (opaque_atoms NoteStatus)
    ; TODO(2018-12-05, https://github.com/ff-notes/ron/issues/115, cblp)
    ; (enum NoteStatus (extends Status) Wiki)

  (struct_set Contact
    #haskell {field_prefix "contact_"}
    status  Status  #ron{merge LWW}
    name    RgaString)

  ; TODO(2019-08-08, #163, cblp) remove a year after release of Track(3)
  ; ff 0.12 is released on 2019-08-14
  (struct_lww TrackV2
    #haskell {field_prefix "trackV2_"}
    provider    String
    source      String
    externalId  String
    url         String)

  (struct_set Track
    #haskell {field_prefix "track_"}
    provider    String  #ron{merge LWW}
    source      String  #ron{merge LWW}
    externalId  String  #ron{merge LWW}
    url         String  #ron{merge LWW})

  (struct_set Tag
    #haskell {field_prefix "tag_"}
    text String #ron{merge LWW})

  ; TODO(2019-08-08, #163, cblp) remove a year after release of Note(3)
  ; ff 0.12 is released on 2019-08-14
  (struct_lww NoteV2
    #haskell {field_prefix "noteV2_"}
    status  NoteStatus
    text    RgaString
    start   Day
    end     Day
    track   TrackV2)

  (struct_set Note
    #haskell {field_prefix "note_"}
    status  NoteStatus                  #ron{merge LWW}
    text    RgaString
    start   Day                         #ron{merge LWW}
    end     Day                         #ron{merge LWW}
    tags    (ObjectRef Tag)             #ron{merge set}
    track   Track
    links   [LinkType (ObjectRef Link)] #ron{merge set})

  (enum LinkType
    SubNote ; a note (target) is a part of another note (source),
            ; e. g. a subtask
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

  fallbackParse = parseNoteV1

instance Collection Contact where
  collectionName = "contact"

instance Collection Tag where
  collectionName = "tag"

data Sample a = Sample {items :: [a], total :: Natural}
  deriving (Eq, Functor, Show)

{- |
  A value identified with some document.
  Should not be used directly, use 'EntityDoc' or 'EntityView' instead.
-}
data Entity doc val
  = Collection doc => Entity {entityId :: DocId doc, entityVal :: val}

deriving instance Eq val => Eq (Entity doc val)

deriving instance Show val => Show (Entity doc val)

type EntityDoc doc = Entity doc doc

type EntityView doc = Entity doc (View doc)

type ContactSample = Sample (EntityDoc Contact)

type NoteSample = Sample (EntityView Note)

emptySample :: Sample a
emptySample = Sample {items = [], total = 0}

-- | Number of notes omitted from the sample.
omitted :: Sample a -> Natural
omitted Sample {total, items} = total - genericLength items

data family View doc

data instance View Note
  = NoteView
      { note :: Note,
        tags :: Set Text
      }
  deriving (Eq, Show)

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
  Overdue _ -> 0
  EndToday -> 1
  EndSoon _ -> 2
  Actual -> 3
  Starting _ -> 4

instance Ord TaskMode where
  Overdue n <= Overdue m = n >= m
  EndSoon n <= EndSoon m = n <= m
  Starting n <= Starting m = n <= m
  m1 <= m2 = taskModeOrder m1 <= taskModeOrder m2

taskMode :: Day -> Note -> TaskMode
taskMode today Note {note_start, note_end} = case note_end of
  Nothing
    | start <= today -> Actual
    | otherwise -> starting start today
  Just e -> case compare e today of
    LT -> overdue today e
    EQ -> EndToday
    GT
      | start <= today -> endSoon e today
      | otherwise -> starting start today
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
  Document {objectFrame} <- loadDocument entityId
  let tryCurrentEncoding = evalObjectState objectFrame readObject
  case tryCurrentEncoding of
    Right entityVal -> pure Entity {entityId, entityVal}
    Left e1 -> do
      let tryNote2Encoding = evalObjectState objectFrame readNoteFromV2
      case tryNote2Encoding of
        Right entityVal -> pure Entity {entityId, entityVal}
        Left e2 -> throwError $ Error "loadNote" [e1, e2]

readNoteFromV2 :: (MonadE m, MonadObjectState a m) => m Note
readNoteFromV2 = do
  ObjectRef uuid <- ask
  NoteV2 {..} <- runReaderT readObject (ObjectRef @NoteV2 uuid)
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
trackFromV2 TrackV2 {..} =
  Track
    { track_externalId = trackV2_externalId,
      track_provider = trackV2_provider,
      track_source = trackV2_source,
      track_url = trackV2_url
    }

-- * Legacy, v1
parseNoteV1 :: MonadE m => UUID -> ByteString -> m (ObjectFrame Note)
parseNoteV1 objectId = liftEitherString . (eitherDecode >=> parseEither p)
  where
    p = withObject "Note" $ \obj -> do
      CRDT.LWW (end :: Maybe Day) endTime <- obj .: "end"
      CRDT.LWW (start :: Day) startTime <- obj .: "start"
      CRDT.LWW (status :: Status) statusTime <- obj .: "status"
      (mTracked :: Maybe JSON.Object) <- obj .:? "tracked"
      text :: CRDT.RgaString <- obj .: "text"
      let endTime' = timeFromV1 endTime
          startTime' = timeFromV1 startTime
          statusTime' = timeFromV1 statusTime
      mTrackObject <-
        case mTracked of
          Nothing -> pure Nothing
          Just tracked -> do
            externalId :: Text <- tracked .: "external_id"
            provider :: Text <- tracked .: "provider"
            source :: Text <- tracked .: "source"
            url :: Text <- tracked .: "url"
            pure
              $ Just
                  ( trackId,
                    mkLww
                      [ Op trackId externalIdName $ toPayload externalId,
                        Op trackId providerName $ toPayload provider,
                        Op trackId sourceName $ toPayload source,
                        Op trackId urlName $ toPayload url
                      ]
                  )
      let frame =
            Map.fromList
              $ [ ( objectId,
                    mkLww
                      $ [Op endTime' endName $ toPayload e | Just e <- [end]]
                        ++ [ Op startTime' startName $ toPayload start,
                             Op statusTime' statusName $ toPayload status,
                             Op objectId textName $ toPayload textId
                           ]
                        ++ [ Op objectId trackName $ toPayload trackId
                             | Just _ <- [mTracked]
                           ]
                  ),
                  (textId, stateToWireChunk $ rgaFromV1 text) -- rgaType
                ]
                ++ maybeToList mTrackObject
      pure ObjectFrame {uuid = objectId, frame}
    mkLww stateBody = WireStateChunk {stateType = lwwType, stateBody}
    textId = UUID.succValue objectId
    trackId = UUID.succValue textId
    endName = $(UUID.liftName "end")
    startName = $(UUID.liftName "start")
    statusName = $(UUID.liftName "status")
    textName = $(UUID.liftName "text")
    trackName = $(UUID.liftName "track")
    externalIdName = $(UUID.liftName "externalId")
    providerName = $(UUID.liftName "provider")
    sourceName = $(UUID.liftName "source")
    urlName = $(UUID.liftName "url")

timeFromV1 :: CRDT.LamportTime -> UUID
timeFromV1 (CRDT.LamportTime unixTime (CRDT.Pid pid)) =
  encodeEvent
    $ Event
        (localEpochTimeFromUnix $ fromIntegral unixTime)
        (applicationSpecific pid)

rgaFromV1 :: CRDT.RgaString -> RgaRep
rgaFromV1 (CRDT.RGA oldRga) =
  stateFromChunk
    [ Op event ref $ toPayload a
      | (vid, a) <- oldRga,
        let event = timeFromV1 vid
            ref = case a of
              '\0' -> UUID.succValue event
              _ -> UUID.zero
    ]

-- used in parseNoteV1
deriveFromJSON defaultOptions ''Status

instance FromJSON NoteStatus where
  parseJSON v = case v of
    "Wiki" -> pure Wiki
    _ -> TaskStatus <$> parseJSON v
