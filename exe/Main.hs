{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Yaml (ToJSON, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      helper, info, metavar, progDesc,
                                      strArgument, subparser, (<**>))
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import           FF (cmdAgenda, cmdNew)

data Cmd = Agenda | New Text

cmdInfo :: ParserInfo Cmd
cmdInfo =
    info (cmdParser <**> helper) $
    fullDesc <> progDesc "A note taker and task tracker"
  where
    cmdParser =
        subparser (command "agenda" cmdAgendaInfo <> command "new" cmdNewInfo)
        <|> cmdAgendaParser
    cmdAgendaInfo = info cmdAgendaParser fullDesc
    cmdAgendaParser = pure Agenda
    cmdNewInfo = info cmdNewParser fullDesc
    cmdNewParser = New <$> strArgument (metavar "TEXT")

main :: IO ()
main = do
    home <- getHomeDirectory
    let dataDir = home </> "Yandex.Disk.localized/Apps/ff"
    cmd <- execParser cmdInfo
    runCmd dataDir cmd

runCmd :: FilePath -> Cmd -> IO ()
runCmd dataDir Agenda = do
    agenda <- cmdAgenda dataDir
    if null agenda then putStrLn "nothing" else yprint agenda
runCmd dataDir (New text) = do
    noteId <- cmdNew dataDir text
    yprint $ object [noteId .= text]

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encodePretty Yaml.defConfig
