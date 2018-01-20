{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import qualified Data.ByteString as BS
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.List (genericLength)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Yaml (ToJSON, Value, encodeFile, object, toJSON, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   getHomeDirectory)
import           System.FilePath (FilePath, takeDirectory, (</>))

import           FF (cmdDone, cmdNew, cmdPostpone, getAgenda)
import           FF.Config (Config (..), appName, getCfgFilePath, loadConfig)
import qualified FF.Config as Config
import           FF.Options (Cmd (..), Config (..), DataDir (..), parseOptions)
import           FF.Types (Agenda (..))

main :: IO ()
main = do
    cfg <- loadConfig
    cmd <- parseOptions
    timeVar <- newTVarIO =<< getRealLocalTime
    runLamportClock timeVar $ runCmd cfg cmd

runCmd :: Config.Config -> Cmd -> LamportClock ()
runCmd cfg@Config.Config{dataDir} cmd = case cmd of
    CmdAgenda showAll -> do
        dir <- checkDataDir
        agenda <- (`runReaderT` dir) $ getAgenda agendaLimit
        yprint $ agendaUI agenda
      where
        agendaLimit = if showAll then Nothing else Just 10
    CmdConfig config -> liftIO $ runCmdConfig config
    CmdDone noteId -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdDone noteId
        yprint $ object ["archived" .= nv]
    CmdNew new -> do
        dir <- checkDataDir
        noteView <- (`runReaderT` dir) $ cmdNew new
        yprint noteView
    CmdPostpone noteId -> do
        dir <- checkDataDir
        nv <- (`runReaderT` dir) $ cmdPostpone noteId
        yprint $ object ["postponed" .= nv]
  where

    checkDataDir :: Monad m => m FilePath
    checkDataDir = case dataDir of
        Just dir -> pure dir
        Nothing  ->
            fail "Data directory isn't set, run `ff config dataDir --help`"

    runCmdConfig Nothing = yprint cfg
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
        yprint $ object ["dataDir" .= dir]
      where
        trySaveDataDir baseDir = do
            guard =<< doesDirectoryExist baseDir
            saveDataDir $ baseDir </> "Apps" </> appName
        saveDataDir dir = do
            cfgFilePath <- getCfgFilePath
            createDirectoryIfMissing True $ takeDirectory cfgFilePath
            encodeFile cfgFilePath cfg{dataDir = Just dir} $> Just dir

yprint :: (ToJSON a, MonadIO io) => a -> io ()
yprint = liftIO . BS.putStr . Yaml.encodePretty config
  where
    config = Yaml.setConfCompare compare Yaml.defConfig

agendaUI :: Agenda -> Value
agendaUI Agenda{notes, total}
    | count == total = toJSON notes
    | otherwise = object
        [ Text.unwords ["nearest", tshow count, "notes"] .= notes
        , Text.unwords ["to see all", tshow total, "notes, run either"]
            .= ["ff -a", "ff agenda --all" :: Text]
        ]
  where
    count = genericLength notes

tshow :: Show a => a -> Text
tshow = Text.pack . show
