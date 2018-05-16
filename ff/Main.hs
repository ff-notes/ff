{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (getRealLocalTime)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   getHomeDirectory)
import           System.FilePath (FilePath, normalise, splitDirectories, (</>))
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDelete, cmdDone, cmdEdit, cmdNew, cmdPostpone,
                     cmdSearch, cmdUnarchive, getSamples, getUtcToday)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Options (Cmd (..), CmdAction (..), DataDir (..),
                             Search (..), Shuffle (..), parseOptions)
import qualified FF.Options as Options
import           FF.Storage (Storage, runStorage)
import           FF.UI (withHeader)
import qualified FF.UI as UI

import           Data.Version (showVersion)
import           Paths_ff (version)

main :: IO ()
main = do
    cfg@Config { ui } <- loadConfig
    cmd               <- parseOptions
    case cmd of
        CmdConfig param  -> runCmdConfig cfg param
        CmdAction action -> do
            timeVar <- newTVarIO =<< getRealLocalTime
            dataDir <- getDataDir cfg
            runStorage dataDir timeVar $ runCmdAction ui action

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

runCmdAction :: ConfigUI -> CmdAction -> Storage ()
runCmdAction ui cmd = do
    today <- getUtcToday
    case cmd of
        CmdAgenda limit -> do
            nvs <- getSamples ui (Just limit) today
            pprint $ UI.samplesInSections limit nvs
        CmdDelete noteId -> do
            nv <- cmdDelete noteId
            pprint $ withHeader "deleted:" $ UI.noteView nv
        CmdDone noteId -> do
            nv <- cmdDone noteId
            pprint $ withHeader "archived:" $ UI.noteView nv
        CmdEdit edit -> do
            nv <- cmdEdit edit
            pprint $ withHeader "edited:" $ UI.noteView nv
        CmdNew new -> do
            nv <- cmdNew new today
            pprint $ withHeader "added:" $ UI.noteView nv
        CmdPostpone noteId -> do
            nv <- cmdPostpone noteId
            pprint $ withHeader "postponed:" $ UI.noteView nv
        CmdSearch (Search text limit) -> do
            nvs <- cmdSearch text (Just limit) today
            pprint $ UI.samplesInSections limit nvs
        CmdUnarchive noteId -> do
            nv <- cmdUnarchive noteId
            pprint . withHeader "unarchived:" $ UI.noteView nv
        CmdVersion -> pprint $ "Version " ++ showVersion version


pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- Terminal.size >>= \case
        Nothing -> pure 80
        Just Terminal.Window { Terminal.width } -> pure width
    putStrLn . pretty width $ ppr a
