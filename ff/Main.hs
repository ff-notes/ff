{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Monad (forever, guard, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either.Extra (fromEither)
import           Data.Foldable (asum, for_)
import           Data.Functor (($>))
import           Data.Maybe (isNothing)
import           Data.Text (snoc)
import           Data.Text.IO (hPutStrLn)
import           Data.Text.Prettyprint.Doc (Doc, LayoutOptions (..),
                                            PageWidth (..), layoutSmart)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            renderStrict)
import           Data.Time (Day)
import           Data.Traversable (for)
import           Data.Version (showVersion)
import           Development.GitRev (gitDirty, gitHash)
import           RON.Storage.IO (DocId (DocId), Storage, runStorage)
import qualified RON.Storage.IO as Storage
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getHomeDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutChar, hPutStr, stderr)
import           System.Pager (printOrPage)

import           FF (cmdDeleteContact, cmdDeleteNote, cmdDone, cmdEdit,
                     cmdNewContact, cmdNewNote, cmdPostpone, cmdSearch, cmdShow,
                     cmdUnarchive, getContactSamples, getDataDir,
                     getTaskSamples, getUtcToday, getWikiSamples,
                     updateTrackedNotes)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Github (getIssueViews, getOpenIssueSamples)
import           FF.Options (Cmd (..), CmdAction (..), Contact (..),
                             DataDir (..), Options (..), Search (..),
                             Shuffle (..), Track (..), parseOptions)
import qualified FF.Options as Options
import           FF.Types (Entity (..))
import           FF.UI (prettyContact, prettyContactSample, prettyNote,
                        prettyNoteList, prettyTaskSections,
                        prettyTasksWikisContacts, prettyWikiSample, withHeader)
import           FF.Upgrade (upgradeDatabase)

import           Paths_ff (version)

main :: IO ()
main = do
    cfg@Config{ui} <- loadConfig
    dataDir <- getDataDir cfg
    h' <- Storage.newHandle dataDir
    Options {..} <- parseOptions h'
    h <- maybe (pure h') Storage.newHandle optionCustomDir
    case optionCmd of
        CmdConfig param  -> runCmdConfig cfg param
        CmdAction action -> runStorage h $ runCmdAction ui action optionBrief
        CmdVersion       -> runCmdVersion

runCmdConfig :: Config -> Maybe Options.Config -> IO ()
runCmdConfig cfg@Config { dataDir, ui } = \case
    Nothing                           -> printConfig cfg
    Just (Options.ConfigDataDir mDir) -> do
        dir <- case mDir of
            Nothing                -> pure dataDir
            Just (DataDirJust dir) -> saveDataDir dir
            Just DataDirYandexDisk -> do
                home <- getHomeDirectory
                asum
                    [ trySaveDataDir $ home </> "Yandex.Disk"
                    , trySaveDataDir $ home </> "Yandex.Disk.localized"
                    , fail "Cant't detect Yandex.Disk directory"
                    ]
        printConfig dir
    Just (Options.ConfigUI mShuffle) -> do
        ui' <- case mShuffle of
            Nothing      -> pure ui
            Just Shuffle -> saveShuffle True
            Just Sort    -> saveShuffle False
        printConfig ui'
  where
    trySaveDataDir baseDir = do
        guard =<< doesDirectoryExist baseDir
        saveDataDir $ baseDir </> "Apps" </> appName
    saveDataDir dir = saveConfig cfg { dataDir = Just dir } $> Just dir
    saveShuffle shuffle' = saveConfig cfg { ui = ui' } $> ui'
        where ui' = ConfigUI {shuffle = shuffle'}

runCmdAction :: ConfigUI -> CmdAction -> Bool -> Storage ()
runCmdAction ui cmd isBrief = do
    today <- getUtcToday
    case cmd of
        CmdAgenda mlimit -> do
            notes <- getTaskSamples ui mlimit today
            pprint $ prettyTaskSections isBrief notes
        CmdContact contact -> cmdContact isBrief contact
        CmdDelete notes ->
            for_ notes $ \noteId -> do
                note <- cmdDeleteNote noteId
                pprint $ withHeader "deleted:" $ prettyNote isBrief note
        CmdDone notes ->
            for_ notes $ \noteId -> do
                note <- cmdDone noteId
                pprint $ withHeader "archived:" $ prettyNote isBrief note
        CmdEdit edit -> do
            notes <- cmdEdit edit
            pprint $ withHeader "edited:" $ prettyNoteList isBrief notes
        CmdNew new -> do
            note <- cmdNewNote new today
            pprint $ withHeader "added:" $ prettyNote isBrief note
        CmdPostpone notes ->
            for_ notes $ \noteId -> do
                note <- cmdPostpone noteId
                pprint $ withHeader "postponed:" $ prettyNote isBrief note
        CmdSearch Search {..} -> do
            (tasks, wikis, contacts) <-
                cmdSearch searchText ui searchLimit today
            pprint $
                prettyTasksWikisContacts
                    isBrief
                    tasks wikis contacts
                    searchTasks searchWiki searchContacts
        CmdShow noteIds -> do
            notes <- for noteIds cmdShow
            pprint $ prettyNoteList isBrief notes
        CmdTrack track ->
            cmdTrack track today isBrief
        CmdUnarchive tasks ->
            for_ tasks $ \taskId -> do
                task <- cmdUnarchive taskId
                pprint . withHeader "unarchived:" $ prettyNote isBrief task
        CmdUpgrade -> do
            upgradeDatabase
            liftIO $ putStrLn "upgraded"
        CmdWiki mlimit -> do
            wikis <- getWikiSamples ui mlimit today
            pprint $ prettyWikiSample isBrief wikis

cmdTrack :: Track -> Day -> Bool -> Storage ()
cmdTrack Track {..} today isBrief =
    if trackDryrun then liftIO $ do
        samples <- run $ getOpenIssueSamples trackAddress trackLimit today
        pprint $ prettyTaskSections isBrief $ (Entity (DocId "") <$>) <$> samples
    else do
        notes <- liftIO $ run $ getIssueViews trackAddress trackLimit
        updateTrackedNotes notes
        liftIO
            $   putStrLn
            $   show (length notes)
                ++ " issues synchronized with the local database"
  where
    run getter = do
        hPutStr stderr "fetching"
        eIssues <- fromEither <$> race
            (runExceptT getter)
            (forever $ hPutChar stderr '.' >> threadDelay 500000)
        hPutStrLn stderr ""
        case eIssues of
            Left err     -> do
                hPutStrLn stderr err
                exitFailure
            Right issues -> pure issues

cmdContact :: Bool -> Maybe Contact -> Storage ()
cmdContact isBrief = \case
    Just (Add name) -> do
        contact <- cmdNewContact name
        pprint $ withHeader "added:" $ prettyContact isBrief contact
    Just (Delete cid) -> do
        contact <- cmdDeleteContact cid
        pprint $ withHeader "deleted:" $ prettyContact isBrief contact
    Nothing -> do
        contacts <- getContactSamples
        pprint $ prettyContactSample isBrief contacts

-- | Template taken from stack:
-- "Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e8 (5807 commits)"
runCmdVersion :: IO ()
runCmdVersion = putStrLn $ concat
    [ "Version ", showVersion version
    , ", Git revision ", $(gitHash)
    , if $(gitDirty) then ", dirty" else ""
    ]

pprint :: MonadIO io => Doc AnsiStyle -> io ()
pprint doc = liftIO $ do
    -- enable colors in `less`
    lessConf <- lookupEnv "LESS"
    when (isNothing lessConf) $ setEnv "LESS" "-R"

    width <- maybe 80 Terminal.width <$> Terminal.size
    printOrPage . (`snoc` '\n') . renderStrict
        $ layoutSmart (LayoutOptions (AvailablePerLine width 1) {- TODO record style -}) doc
