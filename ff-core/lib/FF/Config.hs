{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Config where

import           Control.Exception (throw)
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import qualified Data.ByteString as BS
import           Data.Yaml (FromJSON, ToJSON, decodeFileEither, encodeFile,
                            withObject, (.!=), (.:), (.:?))
import qualified Data.Yaml as Yaml
import           System.Directory (XdgDirectory (XdgConfig),
                                   createDirectoryIfMissing, doesFileExist,
                                   getXdgDirectory)
import           System.FilePath (FilePath, takeDirectory, (</>))

data Config = Config
    { dataDir :: Maybe FilePath
    , isVcs :: Bool
    , ui :: ConfigUI
    }
    deriving (Show)

newtype ConfigUI = ConfigUI
    { shuffle :: Bool
    }
    deriving (Show)

emptyConfig :: Config
emptyConfig = Config
    { dataDir = Nothing
    , ui = defaultConfigUI
    , isVcs = False}

defaultConfigUI :: ConfigUI
defaultConfigUI = ConfigUI {shuffle = False}

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> do
        dataDir <- obj .:? "dataDir"
        isVcs   <- obj .:  "isVcs"
        ui      <- obj .:? "ui" .!= defaultConfigUI
        pure Config{..}

deriveJSON   defaultOptions ''ConfigUI
deriveToJSON defaultOptions ''Config

appName :: String
appName = "ff"

getCfgFilePath :: IO FilePath
getCfgFilePath = getXdgDirectory XdgConfig $ appName </> "config.yaml"

loadConfig :: IO Config
loadConfig = do
    path   <- getCfgFilePath
    exists <- doesFileExist path
    if exists
        then either throw pure =<< decodeFileEither path
        else pure emptyConfig

saveConfig :: Config -> IO ()
saveConfig cfg = do
    cfgFilePath <- getCfgFilePath
    createDirectoryIfMissing True $ takeDirectory cfgFilePath
    encodeFile cfgFilePath cfg

printConfig :: ToJSON a => a -> IO ()
printConfig = BS.putStr . Yaml.encode
