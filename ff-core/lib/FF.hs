{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( cmdDelete
    , cmdDone
    , cmdEdit
    , cmdNew
    , cmdPostpone
    , cmdSearch
    , cmdUnarchive
    , getSamples
    , getUtcToday
    , loadAllNotes
    , loadActiveNotes
    , newNote
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Bifunctor (first)
import           Data.Foldable (asum)
import           Data.List (genericLength, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           Numeric.Natural (Natural)
import           System.Directory (findExecutable)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms)

import           FF.Config (ConfigUI (..))
import           FF.Options (Edit (..), New (..))
import           FF.Storage (Collection, DocId, MonadStorage, Storage,
                             listDocuments, load, modify, saveNew)
import           FF.Types (ModeMap, Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived, Deleted),
                           TaskMode (..), noteView, singletonTaskModeMap)

getSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Natural  -- ^ limit
    -> Day            -- ^ today
    -> m (ModeMap Sample)
getSamples = getSamplesWith $ const True

cmdSearch
    :: Text
    -> Maybe Natural  -- ^ limit
    -> Day            -- ^ today
    -> Storage (ModeMap Sample)
cmdSearch substr = getSamplesWith
    (Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold)
    ConfigUI {shuffle = False}

loadAllNotes :: MonadStorage m => m [NoteView]
loadAllNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure [ noteView doc note | (doc, Just note) <- zip docs mnotes ]

loadActiveNotes :: MonadStorage m => m [NoteView]
loadActiveNotes =
    filter (\NoteView { status } -> status == Active) <$> loadAllNotes

getSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Natural   -- ^ limit
    -> Day             -- ^ today
    -> m (ModeMap Sample)
getSamplesWith predicate ConfigUI { shuffle } limit today = do
    activeNotes <- loadActiveNotes
    pure . takeSamples mGen limit $ splitModes today $ filter
        (\NoteView { text } -> predicate text)
        activeNotes
  where
    mGen | shuffle = Just . mkStdGen . fromIntegral $ toModifiedJulianDay today
         | otherwise = Nothing

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes = foldMap . singletonTaskModeMap

takeSamples
    :: Maybe StdGen
    -> Maybe Natural  -- ^ limit
    -> ModeMap [NoteView]
    -> ModeMap Sample
takeSamples mGen limit modes =
    (`evalState` limit) $ do
        overdue  <- sample end   Overdue
        endToday <- sample end   EndToday
        endSoon  <- sample end   EndSoon
        actual   <- sample start Actual
        starting <- sample start Starting
        pure $ mconcat
            [ overdue
            , endToday
            , endSoon
            , actual
            , starting
            ]
  where
    sample key mode = state $ first mk . \case
        Just n  -> (take (fromIntegral n) xs', Just $ n - len)
        Nothing -> (                      xs', Nothing       )
      where
        xs = fromMaybe [] $ Map.lookup mode modes
        -- in sorting by nid no business-logic is involved,
        -- it's just for determinism
        xs' = case mGen of
            Just gen -> map snd . sortOn fst $ zip (randoms gen :: [Int]) xs
            Nothing  -> sortOn (key &&& nid) xs
        len = genericLength xs
        mk ys = case len of
            0 -> Map.empty
            _ -> Map.singleton mode $ Sample ys len

newNote :: Clock m => Status -> Text -> Day -> Maybe Day -> m Note
newNote status text start end = do
    noteStatus <- LWW.initialize status
    noteText   <- rgaFromText text
    noteStart  <- LWW.initialize start
    noteEnd    <- LWW.initialize end
    pure Note {..}

cmdNew :: MonadStorage m => New -> Day -> m NoteView
cmdNew New { newText, newStart, newEnd } today = do
    let newStart' = fromMaybe today newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    note <- newNote Active newText newStart' newEnd
    nid  <- saveNew note
    pure $ noteView nid note

cmdDelete :: NoteId -> Storage NoteView
cmdDelete nid = modifyAndView nid $ \note@Note {..} -> do
    noteStatus' <- LWW.assign Deleted noteStatus
    noteText'   <- rgaEditText Text.empty noteText
    noteStart'  <- LWW.assign (fromGregorian 0 1 1) noteStart
    noteEnd'    <- LWW.assign Nothing noteEnd
    pure note { noteStatus = noteStatus'
              , noteText   = noteText'
              , noteStart  = noteStart'
              , noteEnd    = noteEnd'
              }

cmdDone :: NoteId -> Storage NoteView
cmdDone nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign Archived noteStatus
    pure note { noteStatus = noteStatus' }

cmdUnarchive :: NoteId -> Storage NoteView
cmdUnarchive nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign Active noteStatus
    pure note { noteStatus = noteStatus' }

cmdEdit :: Edit -> Storage NoteView
cmdEdit (Edit nid Nothing Nothing Nothing) =
    modifyAndView nid $ \note@Note { noteText } -> do
        text'     <- liftIO . runExternalEditor $ rgaToText noteText
        noteText' <- rgaEditText text' noteText
        pure note { noteText = noteText' }
cmdEdit Edit { editId = nid, editEnd, editStart, editText } =
    modifyAndView nid $ \note -> do
        checkStartEnd note
        update note
  where
    checkStartEnd Note { noteStart = (LWW.query -> noteStart), noteEnd } =
        case newStartEnd of
            Just (start, end) -> assertStartBeforeEnd start end
            Nothing           -> pure ()
      where
        newStartEnd = case (editStart, editEnd, LWW.query noteEnd) of
            (Just start, Nothing        , Just end) -> Just (start, end)
            (Nothing   , Just (Just end), _       ) -> Just (noteStart, end)
            (Just start, Just (Just end), _       ) -> Just (start, end)
            _ -> Nothing
    update note@Note { noteEnd, noteStart, noteText } = do
        noteEnd'   <- lwwAssignIfJust editEnd noteEnd
        noteStart' <- lwwAssignIfJust editStart noteStart
        noteText'  <- case editText of
            Nothing -> pure noteText
            Just et -> rgaEditText et noteText
        pure note { noteEnd   = noteEnd'
                  , noteStart = noteStart'
                  , noteText  = noteText'
                  }

lwwAssignIfJust :: Clock m => Maybe a -> LWW a -> m (LWW a)
lwwAssignIfJust = maybe pure LWW.assign

cmdPostpone :: NoteId -> Storage NoteView
cmdPostpone nid = modifyAndView nid $ \note@Note { noteStart, noteEnd } -> do
    today <- getUtcToday
    let start' = addDays 1 $ max today $ LWW.query noteStart
    noteStart' <- LWW.assign start' noteStart
    noteEnd'   <- case LWW.query noteEnd of
        Just end | end < start' -> LWW.assign (Just start') noteEnd
        _                       -> pure noteEnd
    pure note { noteStart = noteStart', noteEnd = noteEnd' }

-- | Check the document exists. Return actual version.
modifyOrFail
    :: (Collection doc, Eq doc)
    => DocId doc
    -> (doc -> Storage doc)
    -> Storage doc
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
    editor <- asum $ assertExecutableFromEnv "EDITOR" : map
        assertExecutable
        ["editor", "micro", "nano"]
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

rgaEditText :: Clock m => Text -> RgaString -> m RgaString
rgaEditText = RGA.edit . Text.unpack

rgaFromText :: Clock m => Text -> m RgaString
rgaFromText = RGA.fromString . Text.unpack

rgaToText :: RgaString -> Text
rgaToText = Text.pack . RGA.toString
