{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Yaml (ToJSON, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import           FF (Result (Result), notes, runAgenda)

main :: IO ()
main = do
    home <- getHomeDirectory
    let notesDir = home </> "Yandex.Disk.localized/Apps/ff"
    Result{notes} <- runAgenda notesDir
    yprint $ object ["agenda" .= notes]

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encodePretty Yaml.defConfig
