{-# LANGUAGE StrictData #-}

module FF.Options
    ( Cmd (..)
    , Config (..)
    , New (..)
    , DataDir (..)
    , Edit (..)
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (auto, command, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, short,
                                      strArgument, strOption, subparser, value,
                                      (<**>))

import           FF.Storage (DocId (DocId))
import           FF.Types (NoteId)

data Cmd
    = CmdAgenda   Limit
    | CmdConfig   (Maybe Config)
    | CmdDelete   NoteId
    | CmdDone     NoteId
    | CmdEdit     Edit
    | CmdNew      New
    | CmdPostpone NoteId
    | CmdSearch   Text Limit

type Limit = Int

newtype Config = ConfigDataDir (Maybe DataDir)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data Edit = Edit
    { editId    :: NoteId
    , editText  :: Maybe Text
    , editStart :: Maybe Day
    , editEnd   :: Maybe (Maybe Day)
    -- ^ Nothing      -- no option            => no change
    --   Just Nothing -- option with tobstone => clear field
    --   Just value   -- option with value    => set field to value
    }
    deriving (Show)

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
        , command "del"       iCmdDelete
        , command "done"      iCmdDone
        , command "edit"      iCmdEdit
        , command "find"      iCmdSearch
        , command "new"       iCmdNew
        , command "postpone"  iCmdPostpone
        ]

    iCmdAgenda    = i pCmdAgenda    "show what you can do right now\
                                    \ [default action]"
    iCmdConfig    = i pCmdConfig    "show/edit configuration"
    iCmdDelete    = i pCmdDelete    "removes task"
    iCmdDone      = i pCmdDone      "mark task done (archive)"
    iCmdEdit      = i pCmdEdit      "edit task"
    iCmdNew       = i pCmdNew       "add new task or note"
    iCmdPostpone  = i pCmdPostpone  "make a task start later"
    iCmdSearch    = i pCmdSearch    "searches for the notes with given text"

    pCmdAgenda    = CmdAgenda   <$> limitOption
    pCmdDelete    = CmdDelete   <$> idArgument
    pCmdDone      = CmdDone     <$> idArgument
    pCmdPostpone  = CmdPostpone <$> idArgument
    pCmdSearch    = CmdSearch   <$> strArgument (metavar "TEXT") <*> limitOption
    pCmdEdit      = CmdEdit     <$> pEdit
    pCmdNew       = CmdNew      <$> pNew

    pNew = New <$> textArgument <*> optional startOption <*> optional endOption
    pEdit = Edit
        <$> idArgument
        <*> optional textOption
        <*> optional startOption
        <*> optional maybeEndOption

    idArgument   = DocId <$> strArgument (metavar "ID"   <> help "note id")
    textArgument =           strArgument (metavar "TEXT" <> help "note text")

    endOption   = dateOption  $ long "end"   <> short 'e' <> help "end date"
    limitOption = option auto $ long "limit" <> short 'l' <> help "limit"
                                <> value 10
    startOption = dateOption  $ long "start" <> short 's' <> help "start date"
    textOption  = strOption   $ long "text"  <> short 't' <> help "note text"
                                <> metavar "TEXT"

    maybeEndOption  = Just <$> endOption
                  <|> flag' Nothing (long "end-clear" <> help "clear end date")

    dateOption m = option auto $ metavar "DATE" <> m

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
