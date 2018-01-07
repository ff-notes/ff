module Options where

import           Control.Applicative ((<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Options.Applicative (ParserInfo, command, fullDesc, helper,
                                      info, metavar, progDesc, strArgument,
                                      subparser, (<**>))

import           FF (DocId (DocId))

data Cmd = Agenda | Dir FilePath | Done DocId | New Text

cmdInfo :: ParserInfo Cmd
cmdInfo =
    info (cmdParser <**> helper) $
    fullDesc <> progDesc "A note taker and task tracker"
  where
    cmdParser =
        subparser (mconcat
            [ command' "agenda" cmdAgendaParser
            , command' "dir"    cmdDirParser
            , command' "done"   cmdDoneParser
            , command' "new"    cmdNewParser
            ])
        <|> cmdAgendaParser
    cmdAgendaParser = pure Agenda
    cmdDirParser    = Dir           <$> strArgument (metavar "DIRECTORY")
    cmdDoneParser   = Done . DocId  <$> strArgument (metavar "ID")
    cmdNewParser    = New           <$> strArgument (metavar "TEXT")
    command' name parser = command name $ info (parser <**> helper) fullDesc
