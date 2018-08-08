{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , loadActiveNotes
    , loadAllNotes
    , newNote
    , splitModes
    , takeSamples
    , updateTrackedNotes
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Extra (unless, void, whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import qualified CRDT.Cv.Max as Max
import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Foldable (asum)
import           Data.List (genericLength, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           System.Directory (findExecutable)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (ConfigUI (..))
import           FF.Options (Edit (..), New (..))
import           FF.Storage (Document (..), MonadStorage, Storage, create,
                             listDocuments, load, modify)
import           FF.Types (Limit, ModeMap, Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived, Deleted),
                           Tracked, noteView, rgaFromText, rgaToText,
                           singletonTaskModeMap)

getSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m (ModeMap Sample)
getSamples = getSamplesWith $ const True

cmdSearch
    :: Text
    -> Maybe Limit
    -> Day  -- ^ today
    -> Storage (ModeMap Sample)
cmdSearch substr = getSamplesWith
    (Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold)
    ConfigUI {shuffle = False}

loadAllNotes :: MonadStorage m => m [NoteView]
loadAllNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure
        [ noteView noteId value
        | (noteId, Right Document{value}) <- zip docs mnotes
        ]

loadTrackedNotes :: MonadStorage m => m [(NoteId, Note)]
loadTrackedNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure
        [ (noteId, value)
        | (noteId, Right Document{value = value @ Note{noteTracked = Just _}})
            <- zip docs mnotes
        ]

loadActiveNotes :: MonadStorage m => m [NoteView]
loadActiveNotes =
    filter (\NoteView { status } -> status == Active) <$> loadAllNotes

getSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap Sample)
getSamplesWith predicate ConfigUI { shuffle } limit today = do
    activeNotes <- loadActiveNotes
    -- in sorting by nid no business-logic is involved,
    -- it's just for determinism
    pure .
        takeSamples limit .
        (if shuffle then shuffleItems gen else fmap (sortOn $ start &&& nid)) .
        splitModes today $
        filter (predicate . text) activeNotes
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today

shuffleItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleItems gen = (`evalState` gen) . traverse shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes today = Map.unionsWith (<>) . fmap (singletonTaskModeMap today)

takeSamples :: Maybe Limit -> ModeMap [NoteView] -> ModeMap Sample
takeSamples Nothing = fmap mkSample
  where
    mkSample ys = Sample ys $ genericLength ys
takeSamples (Just limit) = (`evalState` limit) . traverse takeSample
  where
    takeSample xs =
        state $ \n -> (Sample (take (fromIntegral n) xs) len, n `natSub` len)
      where
        len = genericLength xs
    natSub a b
        | a <= b    = 0
        | otherwise = a - b

updateTrackedNote
    :: [(NoteId, Note)] -- ^ selection of all aready tracked notes
    -> NoteView
    -> Storage ()
updateTrackedNote oldNotes NoteView{..} =
    case sameTrack of
        Nothing -> do
            note <- newNote' status text start end tracked
            void $ create note
        Just (n, _) ->
            void $ modify n $ \note @ Note{..} -> do
                noteStatus' <- lwwAssignIfDiffer status noteStatus
                noteText'   <- rgaEditText text noteText
                pure note{noteStatus = noteStatus', noteText = noteText'}
  where
    isSameTrack oldNote = tracked == (Max.query <$> noteTracked oldNote)
    sameTrack = listToMaybe $ filter (isSameTrack . snd) oldNotes

updateTrackedNotes :: [NoteView] -> Storage ()
updateTrackedNotes nvNews = do
    oldNotes <- loadTrackedNotes
    mapM_ (updateTrackedNote oldNotes) nvNews

-- | Native 'Note' smart constructor
newNote :: Clock m => Status -> Text -> Day -> Maybe Day -> m Note
newNote status text start end = newNote' status text start end Nothing

-- | Generic 'Note' smart constructor
newNote'
    :: Clock m => Status -> Text -> Day -> Maybe Day -> Maybe Tracked -> m Note
newNote' status text start end tracked = do
    noteStatus <- LWW.initialize status
    noteText   <- rgaFromText text
    noteStart  <- LWW.initialize start
    noteEnd    <- LWW.initialize end
    let noteTracked = Max.initial <$> tracked
    pure Note{..}

cmdNew :: MonadStorage m => New -> Day -> m NoteView
cmdNew New { newText, newStart, newEnd } today = do
    let newStart' = fromMaybe today newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    note <- newNote Active newText newStart' newEnd
    nid  <- create note
    pure $ noteView nid note

cmdDelete :: NoteId -> Storage NoteView
cmdDelete nid = modifyAndView nid $ \note@Note {..} -> do
    assertNoteIsNative note
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
    assertNoteIsNative note
    noteStatus' <- LWW.assign Archived noteStatus
    pure note { noteStatus = noteStatus' }

cmdUnarchive :: NoteId -> Storage NoteView
cmdUnarchive nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign Active noteStatus
    pure note { noteStatus = noteStatus' }

cmdEdit :: Edit -> Storage NoteView
cmdEdit Edit{..} = case (editText, editStart, editEnd) of
    (Nothing, Nothing, Nothing) ->
        modifyAndView editId $ \note@Note{noteText} -> do
            assertNoteIsNative note
            text'     <- liftIO . runExternalEditor $ rgaToText noteText
            noteText' <- rgaEditText text' noteText
            pure note{noteText = noteText'}
    _ ->
        modifyAndView editId $ \note -> do
            whenJust editText $ \_ -> assertNoteIsNative note
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
    update :: Note -> Storage Note
    update note @ Note{noteEnd, noteStart, noteText} = do
        noteEnd'   <- lwwAssignIfJust editEnd noteEnd
        noteStart' <- lwwAssignIfJust editStart noteStart
        noteText'  <- maybe (pure noteText) (`rgaEditText` noteText) editText
        pure note
            { noteEnd   = noteEnd'
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

modifyAndView :: NoteId -> (Note -> Storage Note) -> Storage NoteView
modifyAndView nid f = noteView nid <$> modify nid f

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

lwwAssignIfDiffer :: (Eq a, Clock m) => a -> LWW a -> m (LWW a)
lwwAssignIfDiffer new var = do
    let cur = LWW.query var
    if new == cur then
        pure var
    else
        LWW.assign new var

assertNoteIsNative :: Monad m => Note -> m ()
assertNoteIsNative = \case
    Note{noteTracked = Just _} ->
        fail "Oh, no! It is tracked note. Not for modifying. Sorry :("
    _ ->
        pure ()
