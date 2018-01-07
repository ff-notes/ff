{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Exception (throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Yaml (ParseException (InvalidYaml), ToJSON,
                            YamlException (YamlException), decodeFileEither,
                            encodeFile, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (execParser)
import           System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import           System.FilePath (FilePath, (</>))

import           FF (cmdAgenda, cmdDone, cmdNew)

import           Config (Config (..), emptyConfig)
import           Options (Cmd (..), CmdConfig (..), cmdInfo)

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

    cmd <- execParser cmdInfo

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
            "Working directory isn't specified, use `ff config dataDir DIR` to set it"

    runCmdConfig Nothing = yprint cfg
    runCmdConfig (Just (DataDir mdir)) = case mdir of
        Just dir -> encodeFile cfgFile cfg{dataDir = Just dir}
        Nothing  -> yprint $ object ["dataDir" .= dataDir]

yprint :: (ToJSON a, MonadIO io) => a -> io ()
yprint = liftIO . BS.putStr . Yaml.encodePretty Yaml.defConfig
