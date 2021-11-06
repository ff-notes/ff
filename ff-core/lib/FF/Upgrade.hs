{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade
  ( upgradeDatabase
    )
where

import qualified Data.Map.Strict as Map
import           RON.Data (MonadObjectState, getObjectStateChunk,
                           reducibleOpType, stateFromWireChunk,
                           stateToWireChunk)
import           RON.Data.LWW (LwwRep (LwwRep))
import           RON.Data.ORSet (ORSetRep (ORSetRep))
import           RON.Error (Error (Error), MonadE, errorContext, liftMaybe)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Prelude
import           RON.Storage.Backend (MonadStorage, changeDocId, getCollections,
                                      getDocuments)
import           RON.Storage.FS (Collection, DocId, decodeDocId, docIdFromUuid,
                                 modify)
import           RON.Types (Atom (AUuid), ObjectRef (ObjectRef),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk), StateFrame, UUID,
                            WireStateChunk (WireStateChunk, stateType))
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

import           FF.Types (Note)

upgradeDatabase :: (MonadE m, MonadStorage m) => m ()
upgradeDatabase = do
  collections <- getCollections
  for_ collections $ \case
    "note" -> upgradeNoteCollection
    collection -> throwError $ Error ("unsupported type " <> show collection) []

upgradeNoteCollection :: MonadStorage m => m ()
upgradeNoteCollection = do
  docs <- getDocuments @_ @Note
  for_ docs $ \docid -> do
    docid' <- upgradeDocId docid
    modify docid' $ errorContext ("docid' = " <> show docid') $ do
      ObjectRef noteId <- ask
      errorContext "convert note" $ convertLwwToSet noteId
      mTrack <- note_track_get
      whenJust mTrack
        $ errorContext "convert track"
        . convertLwwToSet

convertLwwToSet
  :: (MonadE m, MonadState StateFrame m, ReplicaClock m) => UUID -> m ()
convertLwwToSet uuid =
  errorContext "convertLwwToSet" $ do
    frame <- get
    chunk@WireStateChunk{stateType} <-
      liftMaybe "no such object in chunk" $ Map.lookup uuid frame
    if
      | stateType == lwwType -> doConvert chunk
      | stateType == setType ->
        pure () -- OK
      | otherwise ->
        throwError
          $ Error "bad type"
              ["expected set or lww", Error ("got " <> show stateType) []]
  where
    lwwType = reducibleOpType @LwwRep
    setType = reducibleOpType @ORSetRep

    doConvert chunk = do
      LwwRep lwwRep <- stateFromWireChunk chunk
      opMap <-
        for (Map.assocs lwwRep) $ \(field, Op{payload}) -> do
          opId <- getEventUuid
          pure
            ( opId
            , Op{ opId
                , refId = Zero
                , payload = AUuid field : removeOption payload
                }
            )
      modify'
        $ Map.insert uuid
        $ stateToWireChunk
        $ ORSetRep
        $ Map.fromList opMap

    removeOption = \case
      AUuid u : payload | u == some' -> payload
      [AUuid u] | u == none' -> []
      payload -> payload
      where
        some' = $(UUID.liftName "some")
        none' = $(UUID.liftName "none")

note_track_get :: (MonadE m, MonadObjectState Note m) => m (Maybe UUID)
note_track_get = do
  StateChunk stateBody <- getObjectStateChunk
  pure $
    asum
      [ case payload of
          AUuid field : AUuid ref : _ | field == track -> Just ref
          _ -> Nothing
      | Op{refId = Zero, payload} <- stateBody
      ]
  where
    track = $(UUID.liftName "track")

upgradeDocId :: (Collection a, MonadStorage m) => DocId a -> m (DocId a)
upgradeDocId docid = do
  let mu = decodeDocId docid
  case mu of
    Just (True, _) -> pure docid
    Just (False, uuid) -> do
      let docid' = docIdFromUuid uuid
      changeDocId docid docid'
      pure docid'
    Nothing -> do
      docid' <- docIdFromUuid <$> getEventUuid
      changeDocId docid docid'
      pure docid'
