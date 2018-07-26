{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (forever, guard)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (getRealLocalTime)
import           Data.Either.Extra (fromEither)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Text.IO (hPutStrLn)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as Text
import           Data.Time (Day)
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   getHomeDirectory)
import           System.Exit (exitFailure)
import           System.FilePath (FilePath, normalise, splitDirectories, (</>))
import           System.IO (hPutChar, hPutStr, stderr)
import           Text.PrettyPrint.Mainland (prettyLazyText)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDelete, cmdDone, cmdEdit, cmdNew, cmdPostpone,
                     cmdSearch, cmdUnarchive, getSamples, getUtcToday,
                     updateTracked)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Github (getIssueSamples, getIssueViews)
import           FF.Options (Agenda (..), Cmd (..), CmdAction (..),
                             DataDir (..), Search (..), Shuffle (..),
                             Track (..), parseOptions)
import qualified FF.Options as Options
import           FF.Serve (cmdServe)
import           FF.Storage (Storage, runStorage)
import qualified FF.Storage as Storage
import           FF.UI (withHeader)
import qualified FF.UI as UI
import           System.Pager (printOrPage)

import           Data.Version (showVersion)
import           Development.GitRev (gitDirty, gitHash)
import           Paths_ff (version)

main :: IO ()
main = do
    cfg@Config { ui } <- loadConfig
    cmd               <- parseOptions
    case cmd of
        CmdConfig param  -> runCmdConfig cfg param
        CmdAction action -> do
            hClock <- newTVarIO =<< getRealLocalTime
            hDataDir <- getDataDir cfg
            let h = Storage.Handle{..}
            runStorage h $ runCmdAction h ui action
        CmdVersion -> runCmdVersion

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

checkDataDir :: Monad m => Config -> m FilePath
checkDataDir Config { dataDir } = case dataDir of
    Just dir -> pure dir
    Nothing  -> fail "Data directory isn't set, run `ff config dataDir --help`"

runCmdAction :: Storage.Handle -> ConfigUI -> CmdAction -> Storage ()
runCmdAction h ui cmd = do
    today <- getUtcToday
    case cmd of
        CmdAgenda Agenda {..} -> do
            nvs <- getSamples ui agendaLimit today
            pprint $ UI.prettySamplesBySections agendaShort nvs
        CmdDelete noteId -> do
            nv <- cmdDelete noteId
            pprint $ withHeader "deleted:" $ UI.noteView nv
        CmdDone noteId -> do
            nv <- cmdDone noteId
            pprint $ withHeader "archived:" $ UI.noteView nv
        CmdEdit edit -> do
            nv <- cmdEdit edit
            pprint $ withHeader "edited:" $ UI.noteView nv
        CmdTrack track ->
            cmdTrack track today
        CmdNew new -> do
            nv <- cmdNew new today
            pprint $ withHeader "added:" $ UI.noteView nv
        CmdPostpone noteId -> do
            nv <- cmdPostpone noteId
            pprint $ withHeader "postponed:" $ UI.noteView nv
        CmdSearch Search {..} -> do
            nvs <- cmdSearch searchText searchLimit today
            pprint $ UI.prettySamplesBySections searchShort nvs
        CmdUnarchive noteId -> do
            nv <- cmdUnarchive noteId
            pprint . withHeader "unarchived:" $ UI.noteView nv
        CmdServe -> cmdServe h ui

cmdTrack :: Track -> Day -> Storage ()
cmdTrack Track {..} today =
    if trackDryrun then liftIO $ do
        samples <- run $ getIssueSamples trackAddress trackLimit today
        pprint $ UI.prettySamplesBySections trackShort samples
    else do
        nvs <- liftIO $ run $ getIssueViews trackAddress trackLimit
        updateTracked nvs
        liftIO $ putStrLn $ show (length nvs) ++ " issues copied to local base"
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

-- Template taken from stack:
-- "Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e8 (5807 commits)"
runCmdVersion :: IO ()
runCmdVersion = putStrLn $ concat
    [ "Version ", showVersion version
    , ", Git revision ", $(gitHash)
    , if $(gitDirty) then ", dirty" else ""
    ]

pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- maybe 80 Terminal.width <$> Terminal.size
    printOrPage . toStrict . (`Text.snoc` '\n') . prettyLazyText width $ ppr a
