module FF.Options
    ( Cmd (..)
    , CmdConfig (..)
    , CmdNew (..)
    , DataDir (..)
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Options.Applicative (auto, command, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, short,
                                      strArgument, subparser, (<**>))

import           FF.Storage (DocId (DocId))
import           FF.Types (Note)

data Cmd = Agenda | Config !(Maybe CmdConfig) | Done !(DocId Note) | New !CmdNew

newtype CmdConfig =
    DataDir (Maybe DataDir)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data CmdNew = CmdNew
    { newText   :: !Text
    , newStart  :: !(Maybe Day)
    , newEnd    :: !(Maybe Day)
    }

parseOptions :: IO Cmd
parseOptions = execParser $ i parser "A note taker and task tracker"
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
    pNew    = New <$> pCmdNew
    pCmdNew = CmdNew
        <$> strArgument (metavar "TEXT")
        <*> optional (option auto (long "start" <> short 's' <> metavar "DATE"))
        <*> optional (option auto (long "end"   <> short 'e' <> metavar "DATE"))

    pConfig = Config <$> optional (subparser $ command "dataDir" iDataDir)
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = DataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIR" <> help "path")
            pYandexDisk =
                flag' DataDirYandexDisk $
                long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk"

    i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc
