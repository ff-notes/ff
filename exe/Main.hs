{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Exception (throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import qualified Data.ByteString as BS
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Yaml (ParseException (InvalidYaml), ToJSON,
                            YamlException (YamlException), decodeFileEither,
                            encodeFile, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           System.Directory (XdgDirectory (XdgConfig),
                                   createDirectoryIfMissing, doesDirectoryExist,
                                   getHomeDirectory, getXdgDirectory)
import           System.FilePath (FilePath, takeDirectory, (</>))

import           FF (cmdAgenda, cmdDone, cmdNew)
import           FF.Options (Cmd (..), Config (..), DataDir (..), parseOptions)

import           Config (Config (..), emptyConfig)

appName :: String
appName = "ff"

cfgFilePath :: FilePath
cfgFilePath = appName </> "config.yaml"

main :: IO ()
main = do
    cfgFile <- getXdgDirectory XdgConfig cfgFilePath
    ecfg <- decodeFileEither cfgFile
    cfg <- case ecfg of
        Right cfg                                 -> pure cfg
        Left (InvalidYaml (Just YamlException{})) -> pure emptyConfig
        Left parseException                       -> throw parseException

    cmd <- parseOptions

    timeVar <- newTVarIO =<< getRealLocalTime
    runLamportClock timeVar $ runCmd cfgFile cfg cmd

runCmd :: FilePath -> Config.Config -> Cmd -> LamportClock ()
runCmd cfgFile cfg@Config.Config{dataDir} cmd = case cmd of
    CmdAgenda -> do
        dir <- checkDataDir
        agenda <- (`runReaderT` dir) cmdAgenda
        if null agenda then yprint ("nothing" :: Text) else yprint agenda
    CmdDone noteId -> do
        dir <- checkDataDir
        text <- (`runReaderT` dir) $ cmdDone noteId
        yprint $ object ["archived" .= Map.singleton noteId text]
    CmdNew new -> do
        dir <- checkDataDir
        (noteId, noteView) <- (`runReaderT` dir) $ cmdNew new
        yprint $ Map.singleton noteId noteView
    CmdConfig config -> liftIO $ runCmdConfig config
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
                let ydl = home </> "Yandex.Disk.localized"
                ydlExists <- doesDirectoryExist ydl
                if ydlExists then
                    saveDataDir $ ydl </> "Apps" </> appName
                else
                    fail "Cant't detect Yandex.Disk directory"
        yprint $ object ["dataDir" .= dir]
      where
        saveDataDir dir = do
            createDirectoryIfMissing True $ takeDirectory cfgFile
            encodeFile cfgFile cfg{dataDir = Just dir} $> Just dir

yprint :: (ToJSON a, MonadIO io) => a -> io ()
yprint = liftIO . BS.putStr . Yaml.encodePretty Yaml.defConfig
