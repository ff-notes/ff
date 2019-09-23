{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
  ( cmdDeleteContact,
    cmdDeleteNote,
    cmdDone,
    cmdEdit,
    cmdNewContact,
    cmdNewNote,
    cmdPostpone,
    cmdSearch,
    cmdUnarchive,
    fromRga,
    fromRgaM,
    getContactSamples,
    getDataDir,
    getTaskSamples,
    getUtcToday,
    getWikiSamples,
    loadTasks,
    loadAll,
    loadAllTagTexts,
    loadTagsByRefs,
    noDataDirectoryMessage,
    splitModes,
    sponsors,
    takeSamples,
    toNoteView,
    updateTrackedNotes,
    viewNoteSample,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (filterM, unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, evalState, state)
import Data.Bool (bool)
import Data.Foldable (asum, for_, toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (genericLength, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set, (\\), isSubsetOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (Day, addDays, getCurrentTime, toModifiedJulianDay, utctDay)
import Data.Traversable (for)
import FF.Config (Config (Config), ConfigUI (ConfigUI), dataDir, shuffle)
import FF.Options (Assign (Clear, Set), Edit (..), New (..), assignToMaybe)
import FF.Types
  ( Contact (..),
    ContactId,
    ContactSample,
    Entity (..),
    Limit,
    ModeMap,
    Note (..),
    NoteId,
    NoteSample,
    NoteStatus (..),
    NoteView (..),
    Sample (..),
    Status (..),
    Tag (..),
    Track (..),
    contact_name_clear,
    contact_status_clear,
    emptySample,
    loadNote,
    note_end_clear,
    note_end_read,
    note_end_set,
    note_start_clear,
    note_start_read,
    note_start_set,
    note_status_clear,
    note_status_read,
    note_status_set,
    note_tags_clear,
    note_text_clear,
    note_text_zoom,
    note_track_read,
    taskMode,
  )
import RON.Data
  ( MonadObjectState,
    ObjectStateT,
    evalObjectState,
    newObjectFrame,
    readObject,
  )
import RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import RON.Error (MonadE, throwErrorText)
import RON.Event (ReplicaClock)
import RON.Storage
  ( Collection,
    DocId,
    createDocument,
    decodeDocId,
    docIdFromUuid,
    loadDocument,
    modify,
  )
import RON.Storage.Backend
  ( Document (Document, objectFrame),
    MonadStorage (getDocuments),
  )
import RON.Types (ObjectFrame (ObjectFrame, uuid), ObjectRef (ObjectRef))
import System.Directory
  ( doesDirectoryExist,
    findExecutable,
    getCurrentDirectory,
  )
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), normalise, splitDirectories)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (proc, runProcess)
import System.Random (StdGen, mkStdGen, randoms, split)

load :: (Collection a, MonadStorage m) => DocId a -> m (Entity a)
load docid = do
  Document {objectFrame} <- loadDocument docid
  entityVal <- evalObjectState objectFrame readObject
  pure $ Entity docid entityVal

loadAll :: (Collection a, MonadStorage m) => m [Entity a]
loadAll = getDocuments >>= traverse load

loadAllNotes :: MonadStorage m => m [Entity Note]
loadAllNotes = getDocuments >>= traverse loadNote

searchStatus :: Bool -> Status
searchStatus = bool Active Archived

loadContacts :: MonadStorage m => Bool -> m [Entity Contact]
loadContacts isArchived =
  filter ((== Just (searchStatus isArchived)) . contact_status . entityVal)
    <$> loadAll

-- Load all tags as texts
loadAllTagTexts :: MonadStorage m => m (Set Text)
loadAllTagTexts = Set.fromList . mapMaybe (tag_text . entityVal) <$> loadAll

loadTagRefsByText :: MonadStorage m => Set Text -> m [ObjectRef Tag]
loadTagRefsByText queryTags = do
  allTags <- loadAll
  map (docIdToRef . entityId) <$> filterM compareTags allTags
  where
    compareTags tag = case tag_text $ entityVal tag of
      Nothing -> pure False
      Just txt -> pure $ elem txt queryTags

loadTagsByRefs :: MonadStorage m => [ObjectRef Tag] -> m [Text]
loadTagsByRefs refs = fmap catMaybes $ for refs $ \ref ->
  tag_text . entityVal <$> load (refToDocId ref)

-- | Create tag objects with given texts.
createTags :: MonadStorage m => Set Text -> m [ObjectRef Tag]
createTags tags =
  for (toList tags) $ \tag -> do
    tagFrame@ObjectFrame {uuid} <- newObjectFrame Tag {tag_text = Just tag}
    createDocument tagFrame
    pure $ ObjectRef uuid

-- | Add new tags to Collection of tags.
--
-- It doesn't create tags that are in the collection already.
-- It returns references that should be added to note_tags.
-- References may content ones that note_tags has already.
createNewTags :: MonadStorage m => Set Text -> m [ObjectRef Tag]
createNewTags tags = do
  allTags <- loadAllTagTexts
  existentTagRefs <- loadTagRefsByText tags
  let newTags = tags \\ allTags
  if null newTags
    then pure existentTagRefs
    else do
      createdTags <- createTags newTags
      pure $ existentTagRefs <> createdTags

toNoteView :: MonadStorage m => Entity Note -> m NoteView
toNoteView item = do
  let refs = note_tags $ entityVal item
  tags <- loadTagsByRefs refs
  pure $ NoteView item $ Set.fromList tags

viewNoteSample :: MonadStorage m => Sample (Entity Note) -> m NoteSample
viewNoteSample Sample {items, total} = do
  noteviews <- mapM toNoteView items
  pure $ Sample noteviews total

getContactSamples :: MonadStorage m => Bool -> m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter contacts by text
  -> Bool -- ^ search within archived contacts
  -> m ContactSample
getContactSamplesWith predicate isArchived = do
  contacts <- loadContacts isArchived
  pure . (\ys -> Sample ys $ genericLength ys) $ filter predicate' contacts
  where
    predicate' = predicate . Text.pack . fromRgaM . contact_name . entityVal

fromRga :: RGA a -> [a]
fromRga (RGA xs) = xs

fromRgaM :: Maybe (RGA a) -> [a]
fromRgaM = maybe [] fromRga

loadTasks :: MonadStorage m => Bool -> m [NoteView]
loadTasks inArchived = do
  notes <- loadAllNotes
  let filtered = filter isArchived notes
  traverse toNoteView filtered
  where
    isArchived =
      (Just (TaskStatus $ searchStatus inArchived) ==) . note_status . entityVal

loadWikis :: MonadStorage m => m [Entity Note]
loadWikis = filter ((Just Wiki ==) . note_status . entityVal) <$> loadAllNotes

getTaskSamples
  :: MonadStorage m
  => Bool -- ^ search within archived tasks
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> Set Text -- ^ tags requested
  -> m (ModeMap NoteSample)
getTaskSamples = getTaskSamplesWith $ const True

getTaskSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter notes by text
  -> Bool -- ^ search within archived tasks
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> Set Text -- ^ tags requested
  -> m (ModeMap NoteSample)
getTaskSamplesWith
  predicate
  isArchived
  ConfigUI {shuffle}
  limit
  today
  tagsRequested = do
    allTasks <- loadTasks isArchived
    let tasks =
          filter
            (\NoteView {tags} -> tagsRequested `isSubsetOf` tags)
            allTasks
    pure
      . takeSamples limit
      . shuffleOrSort
      . splitModesBy (entityVal . note) today
      $ filter
          ( predicate
              . Text.pack
              . fromRgaM
              . note_text
              . entityVal
              . note
          )
          tasks
    where
      gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
      shuffleOrSort :: ModeMap [NoteView] -> ModeMap [NoteView]
      shuffleOrSort
        | shuffle = shuffleTraverseItems gen
        | otherwise =
          -- in sorting by entityId no business-logic is involved,
          -- it's just for determinism
          fmap
            $ sortOn
                ( \NoteView {..} ->
                    note_start . entityVal &&& entityId $ note
                )

getWikiSamples
  :: MonadStorage m
  => Bool -- archived search
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m NoteSample
getWikiSamples = getWikiSamplesWith $ const True

getWikiSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter tasks by text
  -> Bool -- ^ if archived search, return Nothing
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m NoteSample
getWikiSamplesWith predicate archive ConfigUI {shuffle} limit today =
  if archive
    then pure emptySample
    else do
      wikis0 <- loadWikis
      let wikis1 = filter predicate' wikis0
      let wikis2 = case limit of
            Nothing -> wikis1
            Just l -> take (fromIntegral l) wikis1
      viewNoteSample $ toSample $ shuffleOrSort wikis2
  where
    predicate' = predicate . Text.pack . fromRgaM . note_text . entityVal
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
      | shuffle = shuffleItems gen
      | otherwise =
        -- in sorting by entityId no business-logic is involved,
        -- it's just for determinism
        sortOn entityId

shuffleItems :: StdGen -> [b] -> [b]
shuffleItems gen = (`evalState` gen) . shuf

shuf :: MonadState StdGen m => [b] -> m [b]
shuf xs = do
  g <- state split
  pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

shuffleTraverseItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleTraverseItems gen = (`evalState` gen) . traverse shuf

splitModesBy :: (note -> Note) -> Day -> [note] -> ModeMap [note]
splitModesBy f today = Map.unionsWith (++) . map singleton
  where
    singleton task = Map.singleton (taskMode today $ f task) [task]

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes = splitModesBy (entityVal . note)

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
      | a <= b = 0
      | otherwise = a - b

updateTrackedNote
  :: MonadStorage m
  => HashMap Track NoteId -- ^ selection of all aready tracked notes
  -> Note -- ^ external note to insert
  -> m ()
updateTrackedNote oldNotes note = case note of
  Note {note_track = Just track} -> case HashMap.lookup track oldNotes of
    Nothing -> do
      obj <- newObjectFrame note
      createDocument obj
    Just noteid -> void $ modify noteid $ do
      note_status_setIfDiffer note_status
      note_text_zoom $ RGA.edit text
  _ -> throwError "External note is expected to be supplied with tracking"
  where
    Note {note_status, note_text = (fromRgaM -> text)} = note

updateTrackedNotes :: MonadStorage m => [Note] -> m ()
updateTrackedNotes newNotes = do
  -- TODO(2018-10-22, https://github.com/ff-notes/ron/issues/116, cblp) index
  -- notes by track in the database and select specific note by its track
  notes <- getDocuments
  oldNotesM <-
    for notes $ \noteId -> do
      Document {objectFrame} <- loadDocument noteId
      mTrack <- evalObjectState objectFrame note_track_read
      pure $ (,noteId) <$> mTrack
  let oldNotes = HashMap.fromList $ catMaybes oldNotesM
  for_ newNotes $ updateTrackedNote oldNotes

cmdNewNote :: MonadStorage m => New -> Day -> m (Entity Note)
cmdNewNote New {text, start, end, isWiki, tags} today = do
  let start' = fromMaybe today start
  whenJust end $ assertStartBeforeEnd start'
  (status, note_end, noteStart) <-
    case end of
      _ | not isWiki -> pure (TaskStatus Active, end, start')
      Nothing -> pure (Wiki, Nothing, today)
      Just _ -> throwError "A wiki must have no end date."
  refs <- createNewTags $ Set.fromList tags
  let note = Note
        { note_end,
          note_start = Just noteStart,
          note_status = Just status,
          note_text = Just $ RGA $ Text.unpack text,
          note_tags = refs,
          note_track = Nothing
        }
  obj@ObjectFrame {uuid} <- newObjectFrame note
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) note

cmdNewContact :: MonadStorage m => Text -> m (Entity Contact)
cmdNewContact name = do
  let contact =
        Contact
          { contact_name = Just $ RGA $ Text.unpack name,
            contact_status = Just Active
          }
  obj@ObjectFrame {uuid} <- newObjectFrame contact
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (Entity Contact)
cmdDeleteContact cid = modifyAndView cid $ do
  contact_status_clear
  contact_name_clear

cmdSearch
  :: MonadStorage m
  => Text -- ^ query
  -> Bool -- ^ search within archived tasks or contacts
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> Set Text -- ^ requested tags
  -> m (ModeMap NoteSample, NoteSample, ContactSample)
cmdSearch substr archive ui limit today tags = do
  -- TODO(cblp, #169, 2018-12-21) search tasks and wikis in one step
  tasks <- getTaskSamplesWith predicate archive ui limit today tags
  wikis <- getWikiSamplesWith predicate archive ui limit today
  contacts <- getContactSamplesWith predicate archive
  pure (tasks, wikis, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

cmdDeleteNote :: MonadStorage m => NoteId -> m (Entity Note)
cmdDeleteNote nid = modifyAndView nid $ do
  assertNoteIsNative
  note_status_clear
  note_text_clear
  note_start_clear
  note_end_clear
  note_tags_clear

cmdDone :: MonadStorage m => NoteId -> m (Entity Note)
cmdDone nid = modifyAndView nid $ do
  assertNoteIsNative
  note_status_set $ TaskStatus Archived

cmdUnarchive :: MonadStorage m => NoteId -> m (Entity Note)
cmdUnarchive nid =
  modifyAndView nid $ note_status_set $ TaskStatus Active

cmdEdit :: (MonadIO m, MonadStorage m) => Edit -> m [Entity Note]
cmdEdit edit = case edit of
  Edit {ids = _ :| _ : _, text = Just _} ->
    throwError "Can't edit content of multiple notes"
  Edit {ids = nid :| [], text, start = Nothing, end = Nothing} ->
    fmap (: []) $ modifyAndView nid $ do
      assertNoteIsNative
      note_text_zoom $ do
        noteText' <-
          case text of
            Just noteText' -> pure noteText'
            Nothing -> do
              noteText <- RGA.getText
              liftIO $ runExternalEditor noteText
        RGA.editText noteText'
  Edit {ids, text, start, end} ->
    fmap toList . for ids $ \nid ->
      modifyAndView nid $ do
        -- check text editability
        whenJust text $ const assertNoteIsNative
        -- check start and end editability
        when (isJust start || isJust end) $ do
          status <- note_status_read
          when (status == Just Wiki)
            $ throwError "Wiki dates are immutable"
        -- check start and end relation
        do
          curStart <- note_start_read
          curEnd <- note_end_read
          let newStartEnd =
                (,)
                  <$> (start <|> curStart)
                  <*> (end' <|> curEnd)
              end' = end >>= assignToMaybe
          whenJust newStartEnd
            $ uncurry assertStartBeforeEnd
        -- update
        whenJust end $ \case
          Clear -> note_end_clear
          Set e -> note_end_set e
        whenJust start note_start_set
        whenJust text $ note_text_zoom . RGA.editText

cmdPostpone :: (MonadIO m, MonadStorage m) => NoteId -> m (Entity Note)
cmdPostpone nid = modifyAndView nid $ do
  today <- getUtcToday
  start <- note_start_read
  let start' = addDays 1 $ maybe today (max today) start
  note_start_set start'
  mEnd <- note_end_read
  case mEnd of
    Just end | end < start' -> note_end_set start'
    _ -> pure ()

modifyAndView
  :: (Collection a, MonadStorage m)
  => DocId a
  -> ObjectStateT a m ()
  -> m (Entity a)
modifyAndView docid f = do
  entityVal <-
    modify docid $ do
      f
      readObject
  pure $ Entity docid entityVal

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

runExternalEditor :: Text -> IO Text
runExternalEditor textOld = do
  editor <-
    asum
      $ assertExecutableFromEnv "EDITOR"
        : map assertExecutable ["editor", "micro", "nano"]
  withSystemTempFile "ff.edit" $ \file fileH -> do
    Text.hPutStr fileH textOld
    hClose fileH
    runProcess (proc editor [file]) >>= \case
      ExitSuccess -> Text.strip <$> Text.readFile file
      ExitFailure {} -> pure textOld
  where
    assertExecutable prog = do
      Just _ <- findExecutable prog
      pure prog
    assertExecutableFromEnv param = assertExecutable =<< getEnv param

assertStartBeforeEnd :: MonadE m => Day -> Day -> m ()
assertStartBeforeEnd start end =
  unless (start <= end) $ throwError "task cannot end before it is started"

note_status_setIfDiffer
  :: (ReplicaClock m, MonadE m, MonadObjectState Note m)
  => Maybe NoteStatus
  -> m ()
note_status_setIfDiffer newStatus = do
  curStatus <- note_status_read
  when (curStatus /= newStatus)
    $ maybe note_status_clear note_status_set newStatus

assertNoteIsNative :: (MonadE m, MonadObjectState Note m) => m ()
assertNoteIsNative = do
  tracking <- note_track_read
  whenJust tracking $ \Track {track_url} ->
    throwErrorText
      $ "A tracked note must be edited in its source"
        <> maybe "" (" :" <>) track_url

getDataDir :: Config -> IO (Maybe FilePath)
getDataDir Config {dataDir} = do
  cur <- getCurrentDirectory
  mDataDirFromVcs <- findVcs $ parents cur
  pure $ mDataDirFromVcs <|> dataDir
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise
    findVcs [] = pure Nothing
    findVcs (dir : dirs) = do
      isDirVcs <- doesDirectoryExist (dir </> ".git")
      if isDirVcs
        then pure . Just $ dir </> ".ff"
        else findVcs dirs

noDataDirectoryMessage :: String
noDataDirectoryMessage =
  "Data directory isn't set, run `ff config dataDir --help`"

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Nothing -> pure ()
  Just x -> f x

sponsors :: [Text]
sponsors = ["Nadezda"]

refToDocId :: ObjectRef a -> DocId a
refToDocId (ObjectRef uid) = docIdFromUuid uid

docIdToRef :: DocId a -> ObjectRef a
docIdToRef docId = case decodeDocId docId of
  Nothing -> error "Decode UUID from DocId failed. DocId is "
  Just (_, uid) -> ObjectRef uid
