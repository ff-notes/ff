{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Config where

import Control.Applicative (empty, (<|>))
import Control.Exception (throw)
import Data.Aeson (FromJSON, withObject, (.!=), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Yaml qualified as Yaml
import System.Directory (
    XdgDirectory (XdgConfig),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getXdgDirectory,
 )
import System.FilePath (normalise, splitDirectories, takeDirectory, (</>))

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
getCfgFilePath = fst <$> loadConfig'

loadConfig :: IO Config
loadConfig = snd =<< loadConfig'

loadConfig' :: IO (FilePath, IO Config)
loadConfig' =
    ( do
        cur <- getCurrentDirectory
        dotFf <- findDotFf $ parents cur
        let path = dotFf </> "config.yaml"
        pure
            ( path
            , do
                exists <- doesFileExist path
                if exists then
                    either throw pure =<< Yaml.decodeFileEither path
                else
                    pure emptyConfig{dataDir = Just dotFf}
            )
    )
        <|> ( do
                path <- getXdgDirectory XdgConfig (appName </> "config.yaml")
                pure
                    ( path
                    , do
                        exists <- doesFileExist path
                        if exists then
                            either throw pure =<< Yaml.decodeFileEither path
                        else
                            pure emptyConfig
                    )
            )
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    findDotFf = \case
        [] -> empty
        dir : dirs -> do
            let dotFfDir = dir </> ".ff"
            isDotFfDir <- doesDirectoryExist dotFfDir
            if isDotFfDir then pure dotFfDir else findDotFf dirs

saveConfig :: Config -> IO ()
saveConfig cfg = do
    cfgFilePath <- getCfgFilePath
    createDirectoryIfMissing True $ takeDirectory cfgFilePath
    JSON.encodeFile cfgFilePath cfg
