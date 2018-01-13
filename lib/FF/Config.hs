{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Config where

import           Control.Exception (throw)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Yaml (decodeFileEither)
import           System.Directory (XdgDirectory (XdgConfig), doesFileExist,
                                   getXdgDirectory)
import           System.FilePath (FilePath, (</>))

newtype Config = Config
    { dataDir :: Maybe FilePath
    }

emptyConfig :: Config
emptyConfig = Config{dataDir = Nothing}

deriveJSON defaultOptions ''Config

appName :: String
appName = "ff"

getCfgFilePath :: IO FilePath
getCfgFilePath = getXdgDirectory XdgConfig $ appName </> "config.yaml"

loadConfig :: IO Config
loadConfig = do
    path <- getCfgFilePath
    exists <- doesFileExist path
    if exists then
        either throw pure =<< decodeFileEither path
    else
        pure emptyConfig
