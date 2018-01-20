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
import qualified CRDT.LWW as LWW
import           Data.List (genericLength, sortOn)
import           Data.Time (Day, addDays, getCurrentTime, utctDay)
import           Data.Traversable (for)

import           FF.Options (New (New), newEnd, newStart, newText)
import           FF.Storage (Collection, DocId, Storage, list, load, save,
                             saveNew)
import           FF.Types (Agenda (..), Note (..), NoteView (..),
                           Status (Active, Archived), noteView)

getAgenda :: Maybe Int -> Storage Agenda
getAgenda mlimit = do
    docs <- list
    mnotes <- for docs load
    let allNotes =
            sortOn
                (\NoteView{start, nid} -> (start, nid))
                [ noteView doc note
                | (doc, Just note@Note{noteStatus = (LWW.query -> Active)}) <-
                    zip docs mnotes
                ]
    pure Agenda
        { notes = case mlimit of
            Nothing    -> allNotes
            Just limit -> take limit allNotes
        , total = genericLength allNotes
        }

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
    let start = LWW.query noteStart
    let start' = 1 `addDays` max today start
    noteStart' <- lift $ LWW.assign start' noteStart  -- TODO LWW.modify
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
