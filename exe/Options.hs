module Options where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Options.Applicative (ParserInfo, command, fullDesc, helper,
                                      metavar, progDesc, strArgument, subparser,
                                      (<**>))
import qualified Options.Applicative as OptApp

import           FF (DocId (DocId))

data Cmd = Agenda | Config !(Maybe CmdConfig) | Done !DocId | New !Text

newtype CmdConfig =
    DataDir (Maybe FilePath)
    -- ^ TODO(cblp, 2018-01-07) add autodetection of dropbox and yadisk

info :: ParserInfo Cmd
info = i parser "A note taker and task tracker"
  where
    parser = subparser commands <|> pAgenda
    commands = mconcat
        [ command "agenda" iAgenda
        , command "config" iConfig
        , command "done"   iDone
        , command "new"    iNew
        ]

    iAgenda = i pAgenda "show what you can do right now [default action]"
    iConfig = i pConfig "show/edit configuration"
    iDone   = i pDone   "mark task done (archive)"
    iNew    = i pNew    "add new task or note"

    pAgenda = pure Agenda
    pDone   = Done . DocId <$> strArgument (metavar "ID")
    pNew    = New <$> strArgument (metavar "TEXT")

    pConfig = Config <$> optional (subparser $ command "dataDir" iDataDir)
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = DataDir <$> optional (strArgument $ metavar "DIR")

    i prsr desc = OptApp.info (prsr <**> helper) $ fullDesc <> progDesc desc
