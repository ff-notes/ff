{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types where

import           Prelude hiding (id)

import           Control.Monad ((>=>))
import qualified CRDT.Cv.RGA as CRDT
import qualified CRDT.LamportClock as CRDT
import qualified CRDT.LWW as CRDT
import           Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject,
                             (.:), (.:?))
import qualified Data.Aeson as JSON
import           Data.Aeson.TH (defaultOptions, deriveFromJSON)
import           Data.Aeson.Types (parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Hashable (Hashable)
import           Data.List (genericLength)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, maybeToList)
import           Data.Text (Text)
import           Data.Time (diffDays)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           RON.Data (Replicated, ReplicatedAsPayload, encoding,
                           fromPayload, mkStateChunk, payloadEncoding,
                           stateFromChunk, stateToChunk, toPayload)
import           RON.Data.LWW (lwwType)
import           RON.Data.RGA (RgaRaw)
import           RON.Data.Time (Day)
import           RON.Epoch (localEpochTimeFromUnix)
import           RON.Error (MonadE, throwErrorString)
import           RON.Event (Event (Event), applicationSpecific, encodeEvent)
import           RON.Schema.TH (mkReplicated)
import           RON.Storage (Collection, DocId, collectionName, fallbackParse)
import           RON.Types (Atom (AUuid), Object (Object, frame, id), Op (Op),
                            UUID)
import qualified RON.UUID as UUID

import           FF.CrdtAesonInstances ()

data NoteStatus = TaskStatus Status | Wiki
    deriving (Eq, Show)

wiki :: UUID
wiki = fromJust $ UUID.mkName "Wiki"

instance Replicated NoteStatus where encoding = payloadEncoding

instance ReplicatedAsPayload NoteStatus where
    toPayload = \case
        TaskStatus status -> toPayload status
        Wiki              -> toPayload wiki
    fromPayload = \case
        [AUuid u] | u == wiki -> pure Wiki
        p                     -> TaskStatus <$> fromPayload p

[mkReplicated|
    (enum Status
        Active Archived Deleted)

    (opaque atoms NoteStatus)
        ; TODO(2018-12-05, cblp) (enum NoteStatus (extends Status) Wiki)

    (struct_lww Contact
        #haskell {field_prefix "contact_"}
        status  Status
        name    RgaString)

    (struct_lww Track
        #haskell {field_prefix "track_"}
        provider    String
        source      String
        externalId  String
        url         String)

    (struct_lww Note
        #haskell {field_prefix "note_"}
        status  NoteStatus
        text    RgaString
        start   Day
        end     (Option Day)
        track   (Option Track))
|]

deriving instance Eq   Contact
deriving instance Show Contact

deriving instance Eq   Note
deriving instance Show Note

deriving instance Bounded Status
deriving instance Enum    Status
deriving instance Eq      Status
deriving instance Show    Status

deriving instance Eq       Track
deriving instance Generic  Track
deriving instance Hashable Track
deriving instance Show     Track

type NoteId = DocId Note

type ContactId = DocId Contact

instance Collection Note where
    collectionName = "note"
    fallbackParse = parseNoteV1

instance Collection Contact where
    collectionName = "contact"

data Sample a = Sample
    { items :: [a]
    , total :: Natural
    }
    deriving (Eq, Functor, Show)

type ContactSample = EntitySample Contact

type NoteSample = EntitySample Note

emptySample :: Sample a
emptySample = Sample{items = [], total = 0}

-- | Number of notes omitted from the sample.
omitted :: Sample a -> Natural
omitted Sample{total, items} = total - genericLength items

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
    = Overdue Natural   -- ^ end in past, with days
    | EndToday          -- ^ end today
    | EndSoon Natural   -- ^ started, end in future, with days
    | Actual            -- ^ started, no end
    | Starting Natural  -- ^ starting in future, with days
    deriving (Eq, Show)

taskModeOrder :: TaskMode -> Int
taskModeOrder = \case
    Overdue _  -> 0
    EndToday   -> 1
    EndSoon _  -> 2
    Actual     -> 3
    Starting _ -> 4

instance Ord TaskMode where
    Overdue  n <= Overdue  m = n >= m
    EndSoon  n <= EndSoon  m = n <= m
    Starting n <= Starting m = n <= m
    m1         <= m2         = taskModeOrder m1 <= taskModeOrder m2

taskMode :: Day -> Note -> TaskMode
taskMode today Note{note_start, note_end} = case note_end of
    Nothing
        | note_start <= today -> Actual
        | otherwise           -> starting note_start today
    Just e -> case compare e today of
        LT -> overdue today e
        EQ -> EndToday
        GT | note_start <= today -> endSoon e today
           | otherwise           -> starting note_start today
  where
    overdue  = helper Overdue
    endSoon  = helper EndSoon
    starting = helper Starting
    helper m x y = m . fromIntegral $ diffDays x y

type ModeMap = Map TaskMode

type Limit = Natural

data Entity a = Entity{entityId :: DocId a, entityVal :: a}
    deriving (Eq, Show)

type EntitySample a = Sample (Entity a)

-- * Legacy, v1

parseNoteV1 :: MonadE m => UUID -> ByteString -> m (Object Note)
parseNoteV1 objectId =
    either throwErrorString pure . (eitherDecode >=> parseEither p)
  where

    p = withObject "Note" $ \obj -> do
        CRDT.LWW (end    :: Maybe Day) endTime    <- obj .:  "end"
        CRDT.LWW (start  :: Day)       startTime  <- obj .:  "start"
        CRDT.LWW (status :: Status)    statusTime <- obj .:  "status"
        (mTracked :: Maybe JSON.Object)           <- obj .:? "tracked"
        text :: CRDT.RgaString <- obj .: "text"
        let endTime'    = timeFromV1 endTime
            startTime'  = timeFromV1 startTime
            statusTime' = timeFromV1 statusTime
        let trackPayload = toPayload $ mTracked $> trackId
        mTrackObject <- case mTracked of
            Nothing -> pure Nothing
            Just tracked -> do
                externalId :: Text <- tracked .: "external_id"
                provider   :: Text <- tracked .: "provider"
                source     :: Text <- tracked .: "source"
                url        :: Text <- tracked .: "url"
                pure $ Just
                    ( trackId
                    , mkStateChunk lwwType
                        [ Op trackId externalIdName $ toPayload externalId
                        , Op trackId providerName   $ toPayload provider
                        , Op trackId sourceName     $ toPayload source
                        , Op trackId urlName        $ toPayload url
                        ]
                    )
        let frame = Map.fromList
                $   [   ( objectId
                        , mkStateChunk lwwType
                            [ Op endTime'    endName    $ toPayload end
                            , Op startTime'  startName  $ toPayload start
                            , Op statusTime' statusName $ toPayload status
                            , Op objectId    textName   $ toPayload textId
                            , Op objectId    trackName    trackPayload
                            ]
                        )
                    ,   (textId, stateToChunk $ rgaFromV1 text) -- rgaType
                    ]
                ++  maybeToList mTrackObject
        pure Object{id = objectId, frame}

    textId  = UUID.succValue objectId
    trackId = UUID.succValue textId

    endName        = fromJust $ UUID.mkName "end"
    startName      = fromJust $ UUID.mkName "start"
    statusName     = fromJust $ UUID.mkName "status"
    textName       = fromJust $ UUID.mkName "text"
    trackName      = fromJust $ UUID.mkName "track"
    externalIdName = fromJust $ UUID.mkName "externalId"
    providerName   = fromJust $ UUID.mkName "provider"
    sourceName     = fromJust $ UUID.mkName "source"
    urlName        = fromJust $ UUID.mkName "url"

timeFromV1 :: CRDT.LamportTime -> UUID
timeFromV1 (CRDT.LamportTime unixTime (CRDT.Pid pid)) =
    encodeEvent $
    Event
        (localEpochTimeFromUnix $ fromIntegral unixTime)
        (applicationSpecific pid)

rgaFromV1 :: CRDT.RgaString -> RgaRaw
rgaFromV1 (CRDT.RGA oldRga) = stateFromChunk
    [ Op event ref $ toPayload a
    | (vid, a) <- oldRga
    , let event = timeFromV1 vid
          ref   = case a of
              '\0' -> UUID.succValue event
              _    -> UUID.zero
    ]

-- used in parseNoteV1
deriveFromJSON defaultOptions ''Status

instance FromJSON NoteStatus where
    parseJSON v = case v of
        "Wiki" -> pure Wiki
        _      -> TaskStatus <$> parseJSON v
