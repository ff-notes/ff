{-# LANGUAGE StrictData #-}

module FF.Options
    ( Cmd (..)
    , Config (..)
    , New (..)
    , DataDir (..)
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (auto, command, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, short,
                                      strArgument, subparser, value, (<**>))

import           FF.Storage (DocId (DocId))
import           FF.Types (NoteId)

data Cmd
    = CmdAgenda   Limit
    | CmdConfig   (Maybe Config)
    | CmdDone     NoteId
    | CmdEdit     NoteId
    | CmdNew      New
    | CmdPostpone NoteId
    | CmdSearch   Text Limit

type Limit = Int

newtype Config = ConfigDataDir (Maybe DataDir)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data New = New
    { newText   :: Text
    , newStart  :: Maybe Day
    , newEnd    :: Maybe Day
    }

parseOptions :: IO Cmd
parseOptions = execParser $ i parser "A note taker and task tracker"
  where
    parser = subparser commands <|> pCmdAgenda
    commands = mconcat
        [ command "agenda"    iCmdAgenda
        , command "config"    iCmdConfig
        , command "done"      iCmdDone
        , command "edit"      iCmdEdit
        , command "find"      iCmdSearch
        , command "new"       iCmdNew
        , command "postpone"  iCmdPostpone
        ]

    iCmdAgenda    = i pCmdAgenda    "show what you can do right now\
                                    \ [default action]"
    iCmdConfig    = i pCmdConfig    "show/edit configuration"
    iCmdDone      = i pCmdDone      "mark task done (archive)"
    iCmdEdit      = i pCmdEdit      "edit task"
    iCmdNew       = i pCmdNew       "add new task or note"
    iCmdPostpone  = i pCmdPostpone  "make a task start later"
    iCmdSearch    = i pCmdSearch    "searches for the notes with given text"

    pCmdAgenda    =
        CmdAgenda <$> option auto (long "limit" <> short 'l' <> value 10)
    pCmdDone      = CmdDone     . DocId <$> strArgument (metavar "ID")
    pCmdEdit      = CmdEdit     . DocId <$> strArgument (metavar "ID")
    pCmdPostpone  = CmdPostpone . DocId <$> strArgument (metavar "ID")
    pCmdSearch    = CmdSearch <$> strArgument (metavar "TEXT") <*> option auto (long "limit" <> short 'l' <> value 10)
    pCmdNew       = CmdNew <$> pNew
    pNew = New
        <$> strArgument (metavar "TEXT")
        <*> optional (option auto (long "start" <> short 's' <> metavar "DATE"))
        <*> optional (option auto (long "end"   <> short 'e' <> metavar "DATE"))

    pCmdConfig = CmdConfig <$> optional (subparser $ command "dataDir" iDataDir)
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIR" <> help "path")
            pYandexDisk =
                flag' DataDirYandexDisk $
                long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk"

    i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc
