{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Yaml (ToJSON)
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative ()
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import           FF (cmdAgenda)

main :: IO ()
main = do
    home <- getHomeDirectory
    let dataDir = home </> "Yandex.Disk.localized/Apps/ff"
    cmdAgenda dataDir >>= yprint

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encodePretty Yaml.defConfig
