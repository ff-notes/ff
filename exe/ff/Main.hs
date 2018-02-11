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
import           System.Directory (doesDirectoryExist, getHomeDirectory)
import           System.FilePath (FilePath, (</>))
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDelete, cmdDone, cmdEdit, cmdNew, cmdPostpone,
                     cmdSearch, getSamples, getUtcToday)
import           FF.Config (Config (..), appName, loadConfig, printConfig,
                            saveConfig)
import           FF.Options (Cmd (..), CmdAction (..), DataDir (..),
                             Search (..), parseOptions)
import qualified FF.Options as Options
import           FF.Storage (Storage, runStorage)
import           FF.UI (withHeader)
import qualified FF.UI as UI

main :: IO ()
main = do
    cfg <- loadConfig
    cmd <- parseOptions
    case cmd of
        CmdConfig param -> runCmdConfig cfg param
        CmdAction action -> do
            timeVar <- newTVarIO =<< getRealLocalTime
            dataDir <- checkDataDir cfg
            runStorage dataDir timeVar $ runCmdAction action

runCmdConfig :: Config -> Maybe Options.Config -> IO ()
runCmdConfig cfg@Config{dataDir} = \case
    Nothing -> printConfig cfg
    Just (Options.ConfigDataDir mdir) -> do
        dir <- case mdir of
            Nothing -> pure dataDir
            Just (DataDirJust dir) -> saveDataDir dir
            Just DataDirYandexDisk -> do
                home <- getHomeDirectory
                asum
                    [ trySaveDataDir $ home </> "Yandex.Disk"
                    , trySaveDataDir $ home </> "Yandex.Disk.localized"
                    , fail "Cant't detect Yandex.Disk directory"
                    ]
        printConfig dir
  where
    trySaveDataDir baseDir = do
        guard =<< doesDirectoryExist baseDir
        saveDataDir $ baseDir </> "Apps" </> appName
    saveDataDir dir = saveConfig cfg{dataDir = Just dir} $> Just dir

checkDataDir :: Monad m => Config -> m FilePath
checkDataDir Config{dataDir} = case dataDir of
    Just dir -> pure dir
    Nothing  ->
        fail "Data directory isn't set, run `ff config dataDir --help`"

runCmdAction :: CmdAction -> Storage ()
runCmdAction cmd = do
    today <- getUtcToday
    case cmd of
        CmdAgenda limit -> do
            nvs <- getSamples limit today
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
            nvs <- cmdSearch text limit today
            pprint $ UI.samplesInSections limit nvs

pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- Terminal.size >>= \case
        Nothing -> pure 80
        Just Terminal.Window{Terminal.width} -> pure width
    putStrLn . pretty width $ ppr a
