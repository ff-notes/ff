{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Config where

import           Control.Exception (throw)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import           Data.Yaml (ToJSON, decodeFileEither, encodeFile)
import qualified Data.Yaml as Yaml
import           System.Directory (XdgDirectory (XdgConfig),
                                   createDirectoryIfMissing, doesFileExist,
                                   getXdgDirectory)
import           System.FilePath (FilePath, takeDirectory, (</>))

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

saveConfig :: Config -> IO ()
saveConfig cfg = do
    cfgFilePath <- getCfgFilePath
    createDirectoryIfMissing True $ takeDirectory cfgFilePath
    encodeFile cfgFilePath cfg

printConfig :: ToJSON a => a -> IO ()
printConfig = BS.putStr . Yaml.encode
