{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Note
    , getSamples
    , getUtcToday
    , cmdDelete
    , cmdDone
    , cmdEdit
    , cmdNew
    , cmdPostpone
    , cmdSearch
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Foldable (asum)
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            utctDay)
import           Data.Traversable (for)
import           System.Directory (findExecutable)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)

import           FF.Options (Edit (..), New (..))
import           FF.Storage (Collection, DocId, MonadStorage, Storage,
                             listDocuments, load, modify, saveNew)
import           FF.Types (ModeMap (..), Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived, Deleted),
                           noteView, singletonTaskModeMap)

getSamples
    :: MonadStorage m
    => Int -- ^ limit
    -> Day -- ^ today
    -> m (ModeMap Sample)
getSamples = getSamplesWith $ const True

cmdSearch :: Text -> Int -> Day -> Storage (ModeMap Sample)
cmdSearch substr =
    getSamplesWith $ Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

getSamplesWith
    :: MonadStorage m
    => (Text -> Bool) -- ^ predicate to filter notes by text
    -> Int -- ^ limit
    -> Day -- ^ today
    -> m (ModeMap Sample)
getSamplesWith predicate limit today = do
    docs <- listDocuments
    mnotes <- for docs load
    let activeNotes =
            [ noteView doc note
            | (doc, Just note@Note{noteStatus, noteText}) <- zip docs mnotes
            , LWW.query noteStatus == Active
            , predicate $ LWW.query noteText
            ]
    pure . takeSamples limit $ splitModes today activeNotes

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes = foldMap . singletonTaskModeMap

takeSamples :: Int -> ModeMap [NoteView] -> ModeMap Sample
takeSamples limit ModeMap{..} = (`evalState` limit) $
    ModeMap
    <$> sample end   overdue
    <*> sample end   endToday
    <*> sample end   endSoon
    <*> sample start actual
    <*> sample start starting
  where
    sample key xs = state $ \n ->
        (Sample (take n xs') (fromIntegral len), n - len)
      where
        -- in sorting by nid no business-logic is involved,
        -- it's just for determinism
        xs' = sortOn (key &&& nid) xs
        len = length xs'

cmdNew :: New -> Storage NoteView
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA getUtcToday newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    note <- do
        noteStatus  <- LWW.initialize Active
        noteText    <- LWW.initialize newText
        noteStart   <- LWW.initialize newStart'
        noteEnd     <- LWW.initialize newEnd
        pure Note{..}
    nid <- saveNew note
    pure $ noteView nid note

cmdDelete :: NoteId -> Storage NoteView
cmdDelete nid =
    modifyAndView nid $ \note@Note{..} -> do
        noteStatus' <- LWW.assign Deleted               noteStatus
        noteText'   <- LWW.assign Text.empty            noteText
        noteStart'  <- LWW.assign (fromGregorian 0 1 1) noteStart
        noteEnd'    <- LWW.assign Nothing               noteEnd
        pure note
            { noteStatus = noteStatus'
            , noteText   = noteText'
            , noteStart  = noteStart'
            , noteEnd    = noteEnd'
            }

cmdDone :: NoteId -> Storage NoteView
cmdDone nid =
    modifyAndView nid $ \note@Note{noteStatus} -> do
        noteStatus' <- LWW.assign Archived noteStatus
        pure note{noteStatus = noteStatus'}

cmdEdit :: Edit -> Storage NoteView
cmdEdit (Edit nid Nothing Nothing Nothing) =
    modifyAndView nid $ \note@Note{noteText} -> do
        text' <- liftIO $ runExternalEditor $ LWW.query noteText
        noteText' <- LWW.assign text' noteText
        pure note{noteText = noteText'}
cmdEdit Edit{editId = nid, editEnd, editStart, editText} =
    modifyAndView nid $ \note -> do
        checkStartEnd note
        update note
  where
    checkStartEnd Note{noteStart = (LWW.query -> noteStart), noteEnd} =
        case newStartEnd of
            Just (start, end) -> assertStartBeforeEnd start end
            Nothing           -> pure ()
      where
        newStartEnd = case (editStart, editEnd, LWW.query noteEnd) of
            (Just start, Nothing        , Just end) -> Just (start    , end)
            (Nothing   , Just (Just end), _       ) -> Just (noteStart, end)
            (Just start, Just (Just end), _       ) -> Just (start    , end)
            _                                       -> Nothing
    update note@Note{noteEnd, noteStart, noteText} = do
        noteEnd'   <- lwwAssignIfJust editEnd   noteEnd
        noteStart' <- lwwAssignIfJust editStart noteStart
        noteText'  <- lwwAssignIfJust editText  noteText
        pure note
            {noteEnd = noteEnd', noteStart = noteStart', noteText = noteText'}

lwwAssignIfJust :: Clock m => Maybe a -> LWW a -> m (LWW a)
lwwAssignIfJust = maybe pure LWW.assign

cmdPostpone :: NoteId -> Storage NoteView
cmdPostpone nid =
    modifyAndView nid $ \note@Note{noteStart, noteEnd} -> do
        today <- getUtcToday
        let start' = addDays 1 $ max today $ LWW.query noteStart
        noteStart' <- LWW.assign start' noteStart
        noteEnd'   <- case LWW.query noteEnd of
            Just end | end < start' -> LWW.assign (Just start') noteEnd
            _                       -> pure                     noteEnd
        pure note{noteStart = noteStart', noteEnd = noteEnd'}

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

runExternalEditor :: Text -> IO Text
runExternalEditor textOld = do
    editor <- asum $
        assertExecutableFromEnv "EDITOR" :
        map assertExecutable ["editor", "micro", "nano"]
    withSystemTempFile "ff.edit" $ \file fileH -> do
        Text.hPutStr fileH textOld
        hClose fileH
        runProcess (proc editor [file]) >>= \case
            ExitSuccess   -> Text.strip <$> Text.readFile file
            ExitFailure{} -> pure textOld
  where
    assertExecutable prog = do
        Just _ <- findExecutable prog
        pure prog
    assertExecutableFromEnv param = assertExecutable =<< getEnv param

assertStartBeforeEnd :: Monad m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ fail "task cannot end before it is started"
