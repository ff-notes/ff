module Options where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Options.Applicative (ParserInfo, command, fullDesc, helper,
                                      info, metavar, progDesc, strArgument,
                                      subparser, (<**>))

import           FF (DocId (DocId))

data Cmd = Agenda | Config !(Maybe CmdConfig) | Done !DocId | New !Text

newtype CmdConfig =
    DataDir (Maybe FilePath)
    -- ^ TODO(cblp, 2018-01-07) add autodetection of dropbox and yadisk

cmdInfo :: ParserInfo Cmd
cmdInfo =
    info (cmdParser <**> helper) $
    fullDesc <> progDesc "A note taker and task tracker"
  where
    cmdParser =
        subparser (mconcat
            [ command' "agenda" cmdAgendaParser "show what you can do right now [default action]"
            , command' "config" cmdConfigParser "show/edit configuration"
            , command' "done"   cmdDoneParser   "mark task done (archive)"
            , command' "new"    cmdNewParser    "add new task or note"
            ])
        <|> cmdAgendaParser

    cmdAgendaParser = pure Agenda
    cmdDoneParser   = Done . DocId <$> strArgument (metavar "ID")
    cmdNewParser    = New <$> strArgument (metavar "TEXT")

    cmdConfigParser =
        Config
        <$> optional
            (subparser $
                command'
                    "dataDir"
                    cmdConfigDataDirParser
                    "the database directory")
    cmdConfigDataDirParser = DataDir <$> optional (strArgument $ metavar "DIR")

    command' name parser desc =
        command name $ info (parser <**> helper) $ fullDesc <> progDesc desc
