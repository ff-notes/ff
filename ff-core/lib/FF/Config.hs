{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Config where

import Control.Exception (throw)
import Data.Aeson (FromJSON, withObject, (.!=), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Yaml qualified as Yaml
import System.Directory (
    XdgDirectory (XdgConfig),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (takeDirectory, (</>))

newtype ConfigUI = ConfigUI
    { shuffle :: Bool
    }
    deriving (Eq, Show)

deriveJSON defaultOptions ''ConfigUI

defaultConfigUI :: ConfigUI
defaultConfigUI = ConfigUI{shuffle = False}

data Config = Config
    { dataDir :: Maybe FilePath
    , externalEditor :: Maybe FilePath
    , ui :: ConfigUI
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON =
        withObject "Config" \obj -> do
            dataDir <- obj .:? "dataDir"
            externalEditor <- obj .:? "externalEditor"
            ui <- obj .:? "ui" .!= defaultConfigUI
            pure Config{..}

deriveToJSON defaultOptions ''Config

emptyConfig :: Config
emptyConfig =
    Config{dataDir = Nothing, externalEditor = Nothing, ui = defaultConfigUI}

appName :: String
appName = "ff"

getCfgFilePath :: IO FilePath
getCfgFilePath = getXdgDirectory XdgConfig $ appName </> "config.yaml"

loadConfig :: IO Config
loadConfig = do
    path <- getCfgFilePath
    exists <- doesFileExist path
    if exists
        then either throw pure =<< Yaml.decodeFileEither path
        else pure emptyConfig

saveConfig :: Config -> IO ()
saveConfig cfg = do
    cfgFilePath <- getCfgFilePath
    createDirectoryIfMissing True $ takeDirectory cfgFilePath
    JSON.encodeFile cfgFilePath cfg
