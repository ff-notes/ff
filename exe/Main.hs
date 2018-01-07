{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Control.Applicative ((<|>))
import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (LamportClock, getRealLocalTime,
                                    runLamportClock)
import           Control.Exception (throw)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Yaml (ToJSON, ParseException(..), YamlException(..), object, (.=), encodeFile, decodeFileEither)
import qualified Data.Yaml.Pretty as Yaml
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      helper, info, metavar, progDesc,
                                      strArgument, subparser, (<**>))
import           System.FilePath (FilePath)
import           System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)

import           FF (DocId (DocId), cmdAgenda, cmdDone, cmdNew)

data Cmd = Agenda | Done DocId | New Text | Dir FilePath

data Config = Config{
    dataDir :: FilePath
}

$(deriveJSON defaultOptions ''Config)

cfgFileName :: FilePath
cfgFileName = "cfg.yaml"

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
    cfgFile <- getXdgDirectory XdgConfig cfgFileName
    ecfg <- decodeFileEither cfgFile
    cmd <- execParser cmdInfo
    timeVar <- newTVarIO =<< getRealLocalTime
    case ecfg of
        Right cfg ->
            runLamportClock timeVar $ runCmd cfgFile (Just cfg) cmd
        Left (InvalidYaml (Just (YamlException _))) ->
            runLamportClock timeVar $ runCmd cfgFile Nothing cmd
        Left parseException ->
            throw parseException

runCmd :: FilePath -> Maybe Config -> Cmd -> LamportClock ()
runCmd cfgFile mcfg cmd =
    case mcfg of
        Just cfg ->
            let dir = dataDir cfg in
            case cmd of
                Dir newDir ->
                    liftIO $ encodeFile cfgFile (cfg{dataDir = newDir})
                Agenda -> liftIO $ do
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
                    liftIO $ encodeFile cfgFile (Config dir)
                _       ->
                    liftIO $ putStrLn "Working directory isn't specified, use *dir* command to specify it"

yprint :: (ToJSON a, MonadIO io) => a -> io ()
yprint = liftIO . BS.putStr . Yaml.encodePretty Yaml.defConfig
