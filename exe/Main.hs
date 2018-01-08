{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Exception (throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import qualified Data.ByteString as BS
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Yaml (ParseException (InvalidYaml), ToJSON,
                            YamlException (YamlException), decodeFileEither,
                            encodeFile, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (execParser)
import           System.Directory (XdgDirectory (XdgConfig),
                                   createDirectoryIfMissing, doesDirectoryExist,
                                   getHomeDirectory, getXdgDirectory)
import           System.FilePath (FilePath, takeDirectory, (</>))

import           FF (cmdAgenda, cmdDone, cmdNew)

import           Config (Config (..), emptyConfig)
import           Options (Cmd (..), CmdConfig (..), DataDir (..), info)

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

    cmd <- execParser info

    timeVar <- newTVarIO =<< getRealLocalTime
    runLamportClock timeVar $ runCmd cfgFile cfg cmd

runCmd :: FilePath -> Config -> Cmd -> LamportClock ()
runCmd cfgFile cfg@Config.Config{dataDir} cmd = case cmd of
    Agenda -> liftIO $ do
        agenda <- cmdAgenda =<< checkDataDir
        if null agenda then putStrLn "nothing" else yprint agenda
    Done noteId -> do
        dir <- checkDataDir
        text <- cmdDone dir noteId
        yprint $ object ["archived" .= Map.singleton noteId text]
    New text -> do
        dir <- checkDataDir
        noteId <- cmdNew dir text
        yprint $ Map.singleton noteId text
    Options.Config cmdConfig -> liftIO $ runCmdConfig cmdConfig
  where

    checkDataDir :: Monad m => m FilePath
    checkDataDir = case dataDir of
        Just dir -> pure dir
        Nothing  -> fail
            "Working directory isn't specified,\
            \ use `ff config dataDir` to set it"

    runCmdConfig Nothing = yprint cfg
    runCmdConfig (Just (DataDir mdir)) = do
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
