{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Agenda
    , Note
    , getAgenda
    , cmdDone
    , cmdNew
    , cmdPostpone
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.List (genericLength, partition, sortOn)
import           Data.Maybe (isJust)
import           Data.Time (Day, addDays, getCurrentTime, utctDay)
import           Data.Traversable (for)

import           FF.Options (New (New), newEnd, newStart, newText)
import           FF.Storage (Collection, DocId, Storage, list, load, save,
                             saveNew)
import           FF.Types (Agenda (..), Note (..), NoteView (..), Sample (..),
                           Status (Active, Archived), noteView)

getAgenda :: Int -> Storage Agenda
getAgenda limit = do
    docs <- list
    mnotes <- for docs load
    let activeNotes =
            [ noteView doc note
            | (doc, Just note@Note{noteStatus = (LWW.query -> Active)}) <-
                zip docs mnotes
            ]
    let (endingNotes, startingNotes) = partition (isJust . end) activeNotes
    pure Agenda
        { ending   = sample limit $ sortOn endingOrder endingNotes
        , starting =
            sample (limit - length endingNotes) $
            sortOn startingOrder startingNotes
        }
  where
    endingOrder NoteView{end, nid} =
        ( end -- closest first
        , nid -- no business-logic involved, just for determinism
        )
    startingOrder NoteView{start, nid} =
        ( start -- oldest first
        , nid   -- no business-logic involved, just for determinism
        )
    sample n xs = Sample (take n xs) (genericLength xs)

cmdNew :: New -> Storage NoteView
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA getUtcToday newStart
    note <- lift $ do
        noteStatus  <- LWW.initial Active
        noteText    <- LWW.initial newText
        noteStart   <- LWW.initial newStart'
        noteEnd     <- LWW.initial newEnd
        pure Note{..}
    nid <- saveNew note
    pure $ noteView nid note

cmdDone :: DocId Note -> Storage NoteView
cmdDone nid = do
    note@Note{noteStatus} <- loadOrFail nid
    noteStatus' <- lift $ LWW.assign Archived noteStatus
    let note' = note{noteStatus = noteStatus'}
    save nid note'
    pure $ noteView nid note'

cmdPostpone :: DocId Note -> Storage NoteView
cmdPostpone nid = do
    note@Note{noteStart} <- loadOrFail nid
    today <- getUtcToday
    noteStart' <- lift $
        lwwModify (\start -> 1 `addDays` max today start) noteStart
    let note' = note{noteStart = noteStart'} -- TODO Storage.modify
    save nid note'
    pure $ noteView nid note'

fromMaybeA :: Applicative m => m a -> Maybe a -> m a
fromMaybeA m = maybe m pure

loadOrFail :: Collection a => DocId a -> Storage a
loadOrFail nid = fromMaybeA (fail msg) =<< load nid  -- TODO ExceptT
  where
    msg = concat
        ["Can't load document ", show nid, ". Where did you get this id?"]

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

lwwModify :: Clock m => (a -> a) -> LWW a -> m (LWW a)
lwwModify f x = LWW.assign (f $ LWW.query x) x
