{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getHomeDirectory)
import           System.FilePath (FilePath, (</>))
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDelete, cmdDone, cmdEdit, cmdNew, cmdPostpone,
                     getAgenda)
import           FF.Config (Config (..), appName, loadConfig, printConfig,
                            saveConfig)
import qualified FF.Config as Config
import           FF.Options (Cmd (..), Config (..), DataDir (..), parseOptions)
import           FF.UI (withHeader)
import qualified FF.UI as UI

main :: IO ()
main = do
    cfg <- loadConfig
    cmd <- parseOptions
    timeVar <- newTVarIO =<< getRealLocalTime
    runLamportClock timeVar $ runCmd cfg cmd

runCmd :: Config.Config -> Cmd -> LamportClock ()
runCmd cfg@Config.Config{dataDir} cmd = case cmd of
    CmdAgenda limit -> do
        dir <- checkDataDir
        agenda <- (`runReaderT` dir) $ getAgenda limit
        pprint $ UI.agenda limit agenda
    CmdConfig config -> liftIO $ runCmdConfig config
    CmdDelete noteId -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdDelete noteId
        pprint $ withHeader "deleted:" $ UI.noteView nv
    CmdDone noteId -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdDone noteId
        pprint $ withHeader "archived:" $ UI.noteView nv
    CmdEdit edit -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdEdit edit
        pprint $ withHeader "edited:" $ UI.noteView nv
    CmdNew new -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdNew new
        pprint $ UI.noteView nv
    CmdPostpone noteId -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdPostpone noteId
        pprint $ withHeader "postponed:" $ UI.noteView nv

  where

    checkDataDir :: Monad m => m FilePath
    checkDataDir = case dataDir of
        Just dir -> pure dir
        Nothing  ->
            fail "Data directory isn't set, run `ff config dataDir --help`"

    runCmdConfig Nothing = printConfig cfg
    runCmdConfig (Just (ConfigDataDir mdir)) = do
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

pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- Terminal.size >>= \case
        Nothing -> pure 80
        Just Terminal.Window{Terminal.width} -> pure width
    putStrLn . pretty width $ ppr a
