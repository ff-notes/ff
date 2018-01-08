module Options where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Options.Applicative (ParserInfo, command, flag', fullDesc,
                                      help, helper, long, metavar, progDesc,
                                      short, strArgument, subparser, (<**>))
import qualified Options.Applicative as OptApp

import           FF (DocId (DocId))

data Cmd = Agenda | Config !(Maybe CmdConfig) | Done !DocId | New !Text

newtype CmdConfig =
    DataDir (Maybe DataDir)
    -- ^ TODO(cblp, 2018-01-07) add autodetection of dropbox and yadisk

data DataDir = DataDirJust FilePath | DataDirYandexDisk

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
        pDataDir = DataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIR" <> help "path")
            pYandexDisk =
                flag' DataDirYandexDisk $
                long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk"

    i prsr desc = OptApp.info (prsr <**> helper) $ fullDesc <> progDesc desc
