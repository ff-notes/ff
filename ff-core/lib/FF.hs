{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module FF (
    cmdDeleteNote,
    cmdDeleteContact,
    cmdDone,
    cmdEdit,
    cmdNewNote,
    cmdNewContact,
    cmdPostpone,
    cmdSearch,
    cmdShow,
    cmdUnarchive,
    getContactSamples,
    getDataDir,
    getTaskSamples,
    getUtcToday,
    getWikiSamples,
    load,
    loadActiveTasks,
    loadAll,
    splitModes,
    takeSamples,
    updateTrackedNotes,
) where

import           Prelude hiding (id)

import           Control.Arrow ((&&&))
import           Control.Monad.Except (MonadError, liftEither, throwError)
import           Control.Monad.Extra (unless, void, when, whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (MonadState, StateT, evalState,
                                             evalStateT, gets, state)
import           Data.Foldable (asum, for_, toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (genericLength, sortOn)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           RON.Data (getObject, newObject)
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaClock)
import           RON.Storage (Collection, DocId (..), Document (..),
                              MonadStorage, createDocument, docIdFromUuid,
                              getDocuments, loadDocument, modify)
import           RON.Storage.IO (Storage)
import           RON.Types (Object, objectId)
import           System.Directory (doesDirectoryExist, findExecutable,
                                   getCurrentDirectory)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.FilePath (normalise, splitDirectories, (</>))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (Config (Config), ConfigUI (ConfigUI), dataDir,
                            shuffle)
import           FF.Options (Edit (..), New (..))
import           FF.Types (Contact (..), ContactId, ContactSample, Entity (..),
                           Limit, ModeMap, Note (..), NoteId, NoteSample,
                           NoteStatus (..), Sample (..), Status (..), Track,
                           contact_name_zoom, contact_status_assign,
                           note_end_assign, note_end_read, note_start_assign,
                           note_start_read, note_status_assign,
                           note_status_read, note_text_zoom, note_track_read,
                           taskMode)

load :: (Collection a, MonadStorage m) => DocId a -> m (Entity a)
load docid = do
    Document{value = obj} <- loadDocument docid
    entityVal <- liftEither $ getObject obj
    pure $ Entity docid entityVal

loadAll :: (Collection a, MonadStorage m) => m [Entity a]
loadAll = getDocuments >>= traverse load

loadActiveContacts :: MonadStorage m => m [Entity Contact]
loadActiveContacts =
    filter ((== Active) . contact_status . entityVal) <$> loadAll

getContactSamples :: MonadStorage m => m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter contacts by text
    -> m ContactSample
getContactSamplesWith predicate = do
    activeContacts <- loadActiveContacts
    pure . (\ys -> Sample ys $ genericLength ys) $
        filter (predicate . Text.pack . contact_name . entityVal) activeContacts

loadActiveTasks :: MonadStorage m => m [Entity Note]
loadActiveTasks =
    filter ((TaskStatus Active ==) . note_status . entityVal) <$> loadAll

loadWikis :: MonadStorage m => m [Entity Note]
loadWikis = filter ((Wiki ==) . note_status . entityVal) <$> loadAll

getTaskSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m (ModeMap NoteSample)
getTaskSamples = getTaskSamplesWith $ const True

getTaskSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap NoteSample)
getTaskSamplesWith predicate ConfigUI{shuffle} limit today = do
    activeTasks <- loadActiveTasks
    pure .
        takeSamples limit .
        shuffleOrSort .
        splitModesBy entityVal today $
        filter (predicate . Text.pack . note_text . entityVal) activeTasks
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
        | shuffle   = shuffleTraverseItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            fmap $ sortOn $ note_start . entityVal &&& entityId

getWikiSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m NoteSample
getWikiSamples = getWikiSamplesWith $ const True

getWikiSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter tasks by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m NoteSample
getWikiSamplesWith predicate ConfigUI{shuffle} limit today = do
    wikis0 <- loadWikis
    let wikis1 = filter (predicate . Text.pack . note_text . entityVal) wikis0
    let wikis2 = case limit of
            Nothing -> wikis1
            Just l -> take (fromIntegral l) wikis1
    pure . toSample $ shuffleOrSort wikis2
  where
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
        | shuffle   = shuffleItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            sortOn entityId

shuffleItems :: StdGen -> [b] -> [b]
shuffleItems gen = (`evalState` gen) . shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

shuffleTraverseItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleTraverseItems gen = (`evalState` gen) . traverse shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

splitModesBy :: (note -> Note) -> Day -> [note] -> ModeMap [note]
splitModesBy f today = Map.unionsWith (++) . map singleton where
    singleton task = Map.singleton (taskMode today $ f task) [task]

splitModes :: Day -> [Note] -> ModeMap [Note]
splitModes = splitModesBy identity

takeSamples :: Maybe Limit -> ModeMap [a] -> ModeMap (Sample a)
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
    :: MonadStorage m
    => HashMap Track NoteId
        -- ^ selection of all aready tracked notes
    -> Note  -- ^ external note to insert
    -> m ()
updateTrackedNote oldNotes note = case note of
    Note{note_track = Just track} -> case HashMap.lookup track oldNotes of
        Nothing -> do
            obj <- newObject note
            createDocument obj
        Just noteid -> void $ modify noteid $ do
            note_status_assignIfDiffer note_status
            note_text_zoom $ RGA.edit note_text
    _ -> throwError "External note is expected to be supplied with tracking"
  where
    Note{note_status, note_text} = note

updateTrackedNotes :: [Note] -> Storage ()
updateTrackedNotes newNotes = do
    -- TODO(2018-10-22, cblp) index notes by track in the database and select
    -- specific note by its track
    notes <- getDocuments
    oldNotes <-
        fmap (HashMap.fromList . catMaybes) .
        for notes $ \noteId -> do
            Document{value = obj} <- loadDocument noteId
            mTrack <- (`evalStateT` obj) note_track_read
            pure $ (, noteId) <$> mTrack
    for_ newNotes $ updateTrackedNote oldNotes

cmdNewNote :: MonadStorage m => New -> Day -> m (Entity Note)
cmdNewNote New{newText, newStart, newEnd, newWiki} today = do
    let newStart' = fromMaybe today newStart
    whenJust newEnd $ assertStartBeforeEnd newStart'
    (note_status, note_end, note_start) <-
        if newWiki then case newEnd of
            Nothing -> pure (Wiki, Nothing, today)
            Just _  -> throwError "A wiki must have no end date."
        else pure (TaskStatus Active, newEnd, newStart')
    let note = Note
            { note_end
            , note_start
            , note_status
            , note_text = Text.unpack newText
            , note_track = Nothing
            }
    obj <- newObject note
    createDocument obj
    pure $ Entity (docIdFromUuid $ objectId obj) note

cmdNewContact :: MonadStorage m => Text -> m (Entity Contact)
cmdNewContact name = do
    let contact =
            Contact{contact_name = Text.unpack name, contact_status = Active}
    obj <- newObject contact
    createDocument obj
    pure $ Entity (docIdFromUuid $ objectId obj) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (Entity Contact)
cmdDeleteContact cid = modifyAndView cid $ do
    contact_status_assign Deleted
    contact_name_zoom $   RGA.editText ""

cmdSearch
    :: Text  -- ^ query
    -> ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> Storage (ModeMap NoteSample, NoteSample, ContactSample)
cmdSearch substr ui limit today = do
    -- TODO(cblp, 2018-12-21) search tasks and wikis in one step
    tasks    <- getTaskSamplesWith    predicate ui limit today
    wikis    <- getWikiSamplesWith    predicate ui limit today
    contacts <- getContactSamplesWith predicate
    pure (tasks, wikis, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

cmdShow :: NoteId -> Storage (Entity Note)
cmdShow = load

cmdDeleteNote :: MonadStorage m => NoteId -> m (Entity Note)
cmdDeleteNote nid = modifyAndView nid $ do
    assertNoteIsNative
    note_status_assign $ TaskStatus Deleted
    note_text_zoom     $ RGA.editText ""
    note_start_assign  $ fromGregorian 0 1 1
    note_end_assign      Nothing

cmdDone :: MonadStorage m => NoteId -> m (Entity Note)
cmdDone nid = modifyAndView nid $ do
    assertNoteIsNative
    note_status_assign $ TaskStatus Archived

cmdUnarchive :: MonadStorage m => NoteId -> m (Entity Note)
cmdUnarchive nid = modifyAndView nid $ note_status_assign $ TaskStatus Active

cmdEdit :: Edit -> Storage [Entity Note]
cmdEdit Edit{ids, text, start, editEnd} =
    case (ids, text, start, editEnd) of
        (_ :| _ : _, Just _, _, _) ->
            throwError "Can't edit content of multiple notes"
        (id :| [], _, Nothing, Nothing) ->
            fmap (:[]) $ modifyAndView id $ do
                assertNoteIsNative
                note_text_zoom $ do
                    noteText' <- case text of
                        Just noteText' -> pure noteText'
                        Nothing        -> do
                            noteText <- liftEither =<< gets RGA.getText
                            liftIO $ runExternalEditor noteText
                    RGA.editText noteText'
        _ ->
            fmap toList . for ids $ \id ->
                modifyAndView id $ do
                    whenJust text $ const assertNoteIsNative
                    checkStartEnd
                    updateStartEndText
  where
    checkStartEnd = do
        nStart <- note_start_read
        mEnd   <- note_end_read
        let newStartEnd = case (start, editEnd, mEnd) of
                (Just eStart, Nothing        , Just end) -> Just (eStart, end)
                (Nothing    , Just (Just end), _       ) -> Just (nStart, end)
                (Just eStart, Just (Just end), _       ) -> Just (eStart, end)
                _                                        -> Nothing
        whenJust newStartEnd $
            uncurry assertStartBeforeEnd

    updateStartEndText = do
        status <- note_status_read
        (start', end) <- case status of
            Wiki -> case (start, editEnd) of
                (Nothing, Nothing) -> pure (Nothing, Nothing)
                _ -> throwError "Wiki dates are immutable"
            _ -> pure (start, editEnd)
        whenJust end    note_end_assign
        whenJust start' note_start_assign
        whenJust text $ note_text_zoom . RGA.editText

cmdPostpone :: NoteId -> Storage (Entity Note)
cmdPostpone nid = modifyAndView nid $ do
    today <- getUtcToday
    start <- note_start_read
    let start' = addDays 1 $ max today start
    note_start_assign start'
    mEnd <- note_end_read
    case mEnd of
        Just end | end < start' -> note_end_assign $ Just start'
        _                       -> pure ()

modifyAndView
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Entity a)
modifyAndView docid f = do
    obj <- modify docid f
    entityVal <- liftEither $ getObject obj
    pure $ Entity docid entityVal

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

assertStartBeforeEnd :: MonadError String m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ throwError "task cannot end before it is started"

note_status_assignIfDiffer
    :: (ReplicaClock m, MonadError String m, MonadState (Object Note) m)
    => NoteStatus -> m ()
note_status_assignIfDiffer newStatus = do
    curStatus <- note_status_read
    when (curStatus /= newStatus) $
        note_status_assign newStatus
{-# ANN note_status_assignIfDiffer ("HLint: ignore Use camelCase" :: String) #-}

assertNoteIsNative :: (MonadError String m, MonadState (Object Note) m) => m ()
assertNoteIsNative = do
    -- TODO(2018-10-22, cblp) use `case of some/none` without full decoding of
    -- `some`
    tracking <- note_track_read
    whenJust tracking $ \_ ->
        throwError "A tracked note must be modified in its source."

getDataDir :: Config -> IO FilePath
getDataDir cfg = do
    cur <- getCurrentDirectory
    mDataDirFromVcs <- findVcs $ parents cur
    case mDataDirFromVcs of
        Just dataDir -> pure dataDir
        Nothing      -> checkDataDir cfg
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise
    findVcs []         = pure Nothing
    findVcs (dir:dirs) = do
        isDirVcs <- doesDirectoryExist (dir </> ".git")
        if isDirVcs then
            pure . Just $ dir </> ".ff"
        else
            findVcs dirs

checkDataDir :: Monad m => Config -> m FilePath
checkDataDir Config{dataDir} = case dataDir of
    Just dir -> pure dir
    Nothing  -> fail "Data directory isn't set, run `ff config dataDir --help`"

identity :: a -> a
identity x = x
