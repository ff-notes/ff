{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Agenda
    , Note
    , getAgenda
    , cmdDone
    , cmdEdit
    , cmdNew
    , cmdPostpone
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.List (genericLength, partition, sortOn)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, getCurrentTime, utctDay)
import           Data.Traversable (for)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)

import           FF.Options (Edit (..), New (..))
import           FF.Storage (Collection, DocId, Storage, list, load, modify,
                             saveNew)
import           FF.Types (Agenda (..), Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived), noteView)

getAgenda :: Int -> Storage Agenda
getAgenda limit = do
    today <- getUtcToday
    let isOverdue NoteView{end} = end < Just today
    let isToday NoteView{end} = end == Just today
    docs <- list
    mnotes <- for docs load
    let activeNotes =
            [ noteView doc note
            | (doc, Just note@Note{noteStatus = (LWW.query -> Active)}) <-
                zip docs mnotes
            ]
    let (notesWithEnd, startingNotes) = partition (isJust . end) activeNotes
    let (overdueNotes, endingNotes) = span isOverdue $ sortOn onEnd notesWithEnd
    let (endingTodayNotes, endingSoonNotes) = span isToday endingNotes
    pure Agenda
        { overdue     = sample limit overdueNotes
        , endingToday = sample (limit - length overdueNotes) endingTodayNotes
        , endingSoon  =
            sample
                (limit - length overdueNotes - length endingTodayNotes)
                endingSoonNotes
        , starting =
            sample
                (limit
                    - length overdueNotes
                    - length endingTodayNotes
                    - length endingSoonNotes)
                (sortOn onStart startingNotes)
        }
  where
    onEnd NoteView{end, nid} =
        ( end -- closest first
        , nid -- no business-logic involved, just for determinism
        )
    onStart NoteView{start, nid} =
        ( start -- oldest first
        , nid   -- no business-logic involved, just for determinism
        )
    sample n xs = Sample (take n xs) (genericLength xs)

cmdNew :: New -> Storage NoteView
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA getUtcToday newStart
    note <- do
        noteStatus  <- LWW.initial Active
        noteText    <- LWW.initial newText
        noteStart   <- LWW.initial newStart'
        noteEnd     <- LWW.initial newEnd
        pure Note{..}
    nid <- saveNew note
    pure $ noteView nid note

cmdDone :: NoteId -> Storage NoteView
cmdDone nid =
    modifyAndView nid $ \note@Note{noteStatus} -> do
        noteStatus' <- LWW.assign Archived noteStatus
        pure note{noteStatus = noteStatus'}

cmdEdit :: Edit -> Storage NoteView
cmdEdit (Edit nid Nothing Nothing Nothing) =
    modifyAndView nid $ \note@Note{noteText} -> do
        text' <- liftIO $ runExternalEditor $ LWW.query noteText
        noteText' <- lwwModify (const text') noteText
        pure note{noteText = noteText'}
cmdEdit Edit{editId = nid, editEnd, editStart, editText} =
    modifyAndView nid $ \note@Note{noteEnd, noteStart, noteText} -> do
        noteEnd'   <- lwwModify (`fromMaybe` editEnd)   noteEnd
        noteStart' <- lwwModify (`fromMaybe` editStart) noteStart
        noteText'  <- lwwModify (`fromMaybe` editText)  noteText
        pure note
            {noteEnd = noteEnd', noteStart = noteStart', noteText = noteText'}

cmdPostpone :: NoteId -> Storage NoteView
cmdPostpone nid =
    modifyAndView nid $ \note@Note{noteStart} -> do
        today <- getUtcToday
        noteStart' <-
            lwwModify (\start -> 1 `addDays` max today start) noteStart
        pure note{noteStart = noteStart'}

fromMaybeA :: Applicative m => m a -> Maybe a -> m a
fromMaybeA m = maybe m pure

-- | Check the document exists. Return actual version.
modifyOrFail ::
    (Collection doc, Eq doc) => DocId doc -> (doc -> Storage doc) -> Storage doc
modifyOrFail docId f = modify docId $ \case
    Nothing -> fail $ concat
        ["Can't load document ", show docId, ". Where did you get this id?"]
    Just docOld -> do
        docNew <- f docOld
        pure (docNew, docNew)

modifyAndView :: NoteId -> (Note -> Storage Note) -> Storage NoteView
modifyAndView nid f = noteView nid <$> modifyOrFail nid f

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

lwwModify :: (Eq a, Clock m) => (a -> a) -> LWW a -> m (LWW a)
lwwModify f lww = let
    x = LWW.query lww
    y = f x
    in
    if x /= y then LWW.assign y lww else pure lww

runExternalEditor :: Text -> IO Text
runExternalEditor textOld =
    withSystemTempFile "ff.edit" $ \file fileH -> do
        Text.hPutStr fileH textOld
        hClose fileH
        runProcess (proc editor [file]) >>= \case
            ExitSuccess   -> Text.strip <$> Text.readFile file
            ExitFailure{} -> pure textOld

editor :: FilePath
editor = "nano"
