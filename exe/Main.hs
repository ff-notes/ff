{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Yaml (ToJSON, FromJSON, object, (.=), encodeFile, decodeFileEither)
import qualified Data.Yaml.Pretty as Yaml
import           GHC.Generics (Generic)
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      helper, info, metavar, progDesc,
                                      strArgument, subparser, (<**>))
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>), FilePath)

import           FF (DocId (DocId), cmdAgenda, cmdDone, cmdNew)

data Cmd = Agenda | Done DocId | New Text | Dir FilePath

data Config = Config{
    dataDir :: FilePath
} deriving Generic

instance FromJSON Config
instance ToJSON Config

cfgFile :: FilePath
cfgFile = "cfg.yaml"

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
            , command' "dir"    cmdDirParser
            ])
        <|> cmdAgendaParser
    cmdAgendaParser = pure Agenda
    cmdDoneParser   = Done . DocId  <$> strArgument (metavar "ID")
    cmdNewParser    = New           <$> strArgument (metavar "TEXT")
    cmdDirParser    = Dir           <$> strArgument (metavar "DIRECTORY")
    command' name parser = command name $ info (parser <**> helper) fullDesc

main :: IO ()
main = do
    ecfg <- decodeFileEither cfgFile
    cmd <- execParser cmdInfo
    case ecfg of
        Right cfg ->
            runCmd (Just cfg) cmd
        Left _    ->
            runCmd Nothing cmd

runCmd :: Maybe Config -> Cmd -> IO ()
runCmd mcfg cmd =
    case mcfg of
        Just cfg ->
            let dir = dataDir cfg in
            case cmd of
                Dir newDir ->
                    encodeFile cfgFile (cfg{dataDir = newDir})
                Agenda -> do
                    agenda <- cmdAgenda dir
                    if null agenda then putStrLn "nothing" else yprint agenda
                Done noteId -> do
                    text <- cmdDone dir noteId
                    yprint $ object ["archived" .= Map.singleton noteId text]
                New text -> do
                    noteId <- cmdNew dir text
                    yprint $ Map.singleton noteId text
        Nothing  ->
            case cmd of
                Dir dir ->
                    encodeFile cfgFile (Config dir)
                _       ->
                    putStrLn "Working directory isn't specified, use *dir* command to specify it"

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encodePretty Yaml.defConfig
