{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Yaml (ToJSON, object, (.=))
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      helper, info, metavar, progDesc,
                                      strArgument, subparser, (<**>))
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import           FF (DocId (DocId), cmdAgenda, cmdDone, cmdNew)

data Cmd = Agenda | Done DocId | New Text

cmdInfo :: ParserInfo Cmd
cmdInfo =
    info (cmdParser <**> helper) $
    fullDesc <> progDesc "A note taker and task tracker"
  where
    cmdParser =
        subparser (mconcat
            [ command' "agenda" cmdAgendaParser
            , command' "done"   cmdDoneParser
            , command' "new"    cmdNewParser
            ])
        <|> cmdAgendaParser
    cmdAgendaParser = pure Agenda
    cmdDoneParser   = Done . DocId  <$> strArgument (metavar "ID")
    cmdNewParser    = New           <$> strArgument (metavar "TEXT")
    command' name parser = command name $ info (parser <**> helper) fullDesc

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
    yprint $ Map.singleton noteId text
runCmd dataDir (Done noteId) = do
    text <- cmdDone dataDir noteId
    yprint $ object ["archived" .= Map.singleton noteId text]

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encodePretty Yaml.defConfig
