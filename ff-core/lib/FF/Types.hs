{-# OPTIONS -Wwarn=orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import Control.Monad.Except (throwError)
import Data.Aeson qualified as JSON
import Data.Aeson.Extra (
    ToJSON,
    fieldLabelModifier,
    singletonObjectSum,
    toJSON,
    (.=),
 )
import Data.Aeson.Key qualified as JSON.Key
import Data.Aeson.KeyMap qualified as JSON.KeyMap
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Aeson.Types qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (fold)
import Data.Hashable (Hashable)
import Data.List (genericLength)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, diffDays)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import RON.Data (
    Replicated,
    ReplicatedAsPayload,
    evalObjectState,
    fromPayload,
    payloadEncoding,
    readObject,
    toPayload,
 )
import RON.Data qualified
import RON.Data.RGA (RGA)
import RON.Data.Time (Day)
import RON.Error (Error (Error))
import RON.Schema.TH (mkReplicated)
import RON.Storage (Collection, DocId, collectionName, loadDocument)
import RON.Storage.Backend (DocId (DocId), DocVersion, MonadStorage)
import RON.Storage.Backend qualified
import RON.Text.Serialize (serializeUuid)
import RON.Types (Atom (AUuid), ObjectRef, UUID)
import RON.UUID qualified as UUID

deriveToJSON defaultOptions ''RGA

uuidToText :: UUID -> Text
uuidToText = Text.decodeUtf8 . BSL.toStrict . serializeUuid

instance ToJSON UUID where
    toJSON = JSON.String . uuidToText

deriveToJSON defaultOptions ''ObjectRef

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

    (struct_set Track
        #haskell {field_prefix "track_"}
        (provider   lww String)
        (source     lww String)
        (externalId lww String)
        (url        lww String)
    )

    (struct_set Tag
        #haskell {field_prefix "tag_"}
        (text   lww     String)
        (desc   merge   RgaString)
    )

    (struct_set Note
        #haskell {field_prefix "note_"}
        (status     lww     NoteStatus)
        (text       merge   RgaString)
        (start      lww     Day)
        (end        lww     Day)
        (tags       set     (ObjectRef Tag))
        (track      merge   Track)
        (links      set     (ObjectRef Link))
        (recurring  lww     Bool)
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

    (struct_set TagGroup
        #haskell {field_prefix "tagGroup_"}
        (name       lww String)
        (exclusive  lww Bool)
        (member     set (ObjectRef Tag))
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

instance Collection TagGroup where
    collectionName = "tag_group"

data Sample a = Sample {items :: [a], total :: Natural}
    deriving (Eq, Functor, Show)

instance (ToJSON a) => ToJSON (Sample a) where
    toJSON Sample{items} = toJSON items

{- |
  A value identified with some document.
  Should not be used directly, use 'EntityDoc' or 'EntityView' instead.
-}
data Entity a val
    = (Collection a) =>
    Entity
    { entityId :: DocId a
    , entityVal :: val
    , entityVersion :: DocVersion
    }

deriving instance (Eq val) => Eq (Entity doc val)

deriving instance (Show val) => Show (Entity doc val)

instance (ToJSON val) => ToJSON (Entity doc val) where
    toJSON e = JSON.object [entityToJson e]
    toJSONList = JSON.object . map entityToJson

entityToJson :: (ToJSON val) => Entity doc val -> JSON.Pair
entityToJson Entity{entityId = DocId entityId, entityVal} = key .= entityVal
  where
    key =
        JSON.Key.fromText $
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
    , tags :: Map Text Text
    -- ^ the key is UUID or URI of the tag
    , created :: UTCTime
    , lastUpdated :: UTCTime
    }
    deriving (Eq, Show)

$( fold
    <$> for
        [ (''Note, defaultOptions{fieldLabelModifier = drop 5})
        , (''NoteStatus, singletonObjectSum)
        , (''Status, defaultOptions)
        , (''Tag, defaultOptions{fieldLabelModifier = drop 4})
        , (''Track, defaultOptions{fieldLabelModifier = drop 6})
        ]
        \(name, options) -> deriveToJSON options name
 )

instance ToJSON (View Note) where
    toJSON NoteView{note, tags} =
        modifyObject (JSON.KeyMap.insert "tags" tags') $ toJSON note
      where
        tags' =
            JSON.Object
                . JSON.KeyMap.fromMap
                . Map.mapKeysMonotonic JSON.Key.fromText
                . fmap JSON.String
                $ tags

modifyObject :: (JSON.Object -> JSON.Object) -> JSON.Value -> JSON.Value
modifyObject f val = case val of
    JSON.Object obj -> JSON.Object $ f obj
    _ -> error "Note must be serialized to Object"

type ModeMap = Map TaskMode

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
    = -- | end in past, with days
      Overdue Natural
    | -- | end today
      EndToday
    | -- | started, end in future, with days
      EndSoon Natural
    | -- | started, no end
      Actual
    | -- | starting in future, with days
      Starting Natural
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
    n <= m = taskModeOrder n <= taskModeOrder m

taskMode :: Day -> Note -> TaskMode
taskMode today Note{note_start, note_end} =
    case note_end of
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

loadNote :: (MonadStorage m) => NoteId -> m (EntityDoc Note)
loadNote entityId = do
    document <- loadDocument entityId
    let tryCurrentEncoding = evalObjectState document.objectFrame readObject
    case tryCurrentEncoding of
        Right entityVal ->
            pure
                Entity
                    { entityId
                    , entityVal
                    , entityVersion = maximum document.versions
                    }
        Left e1 -> throwError $ Error "loadNote" [e1]

deriveToJSON defaultOptions ''Contact
deriveToJSON defaultOptions ''TaskMode
