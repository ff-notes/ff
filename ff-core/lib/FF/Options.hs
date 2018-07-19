{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module FF.Options
    ( Cmd (..)
    , CmdAction (..)
    , Config (..)
    , DataDir (..)
    , Edit (..)
    , New (..)
    , Search (..)
    , Shuffle (..)
    , Track (..)
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (auto, command, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, short,
                                      strArgument, strOption, subparser, switch,
                                      (<**>))

import           FF.Storage (DocId (DocId))
import           FF.Types (Limit, NoteId)

data Cmd
    = CmdConfig (Maybe Config)
    | CmdAction CmdAction
    | CmdVersion

data CmdAction
    = CmdAgenda     (Maybe Limit)
    | CmdDelete     NoteId
    | CmdDone       NoteId
    | CmdEdit       Edit
    | CmdTrack      Track
    | CmdNew        New
    | CmdPostpone   NoteId
    | CmdSearch     Search
    | CmdUnarchive  NoteId
    | CmdServe

data Track = Track
    { dryrun  :: Bool
    , address :: Maybe Text
    , limit   :: Maybe Limit
    }

data Config = ConfigDataDir (Maybe DataDir) | ConfigUI (Maybe Shuffle)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data Shuffle = Shuffle | Sort

data Edit = Edit
    { editId    :: NoteId
    , editText  :: Maybe Text
    , editStart :: Maybe Day
    , editEnd   :: Maybe (Maybe Day)
    -- ^ Nothing      -- no option             => no change
    --   Just Nothing -- option with tombstone => clear field
    --   Just value   -- option with value     => set field to value
    }
    deriving (Show)

data New = New
    { newText   :: Text
    , newStart  :: Maybe Day
    , newEnd    :: Maybe Day
    }

data Search = Search Text (Maybe Limit)

parseOptions :: IO Cmd
parseOptions = execParser $ i parser "A note taker and task tracker"
  where
    parser   = version <|> subparser commands <|> cmdAgenda
    commands = mconcat
        [ command "add"       iCmdAdd
        , command "agenda"    iCmdAgenda
        , command "config"    iCmdConfig
        , command "delete"    iCmdDelete
        , command "done"      iCmdDone
        , command "edit"      iCmdEdit
        , command "new"       iCmdNew
        , command "postpone"  iCmdPostpone
        , command "search"    iCmdSearch
        , command "serve"     iCmdServe
        , command "track"     iCmdTrack
        , command "unarchive" iCmdUnarchive
        , command "serve"     iCmdServe
        ]

    iCmdAdd       = i cmdNew        "add a new task or note"
    iCmdAgenda    = i cmdAgenda     "show what you can do right now\
                                    \ [default action]"
    iCmdConfig    = i cmdConfig    "show/edit configuration"
    iCmdDelete    = i cmdDelete    "delete a task"
    iCmdDone      = i cmdDone      "mark a task done (archive)"
    iCmdEdit      = i cmdEdit      "edit a task or a note"
    iCmdTrack     = i cmdTrack     "track issues from GitHub"
    iCmdNew       = i cmdNew       "synonym for `add`"
    iCmdPostpone  = i cmdPostpone  "make a task start later"
    iCmdSearch    = i cmdSearch    "search for notes with the given text"
    iCmdUnarchive = i cmdUnarchive "restore the note from archive"
    iCmdServe     = i cmdServe     "serve application through the http"

    cmdAgenda    = CmdAction . CmdAgenda    <$> optional limit
    cmdDelete    = CmdAction . CmdDelete    <$> noteid
    cmdDone      = CmdAction . CmdDone      <$> noteid
    cmdEdit      = CmdAction . CmdEdit      <$> edit
    cmdTrack     = CmdAction . CmdTrack     <$> track
    cmdNew       = CmdAction . CmdNew       <$> new
    cmdPostpone  = CmdAction . CmdPostpone  <$> noteid
    cmdSearch    = CmdAction . CmdSearch    <$> search
    cmdUnarchive = CmdAction . CmdUnarchive <$> noteid
    cmdServe     = pure $ CmdAction CmdServe

    track = Track
        <$> pDryRun
        <*> optional pRepo
        <*> optional limit

    pDryRun = switch
        (long "dry-run" <> short 'd' <>
        help "List issues from github")

    pRepo  = strOption $
        long "repo" <> short 'r' <> metavar "USER/REPO" <>
        help "User or organization/repository"
    new = New <$> text <*> optional start <*> optional end
    edit = Edit
        <$> noteid
        <*> optional textOption
        <*> optional start
        <*> optional maybeEnd
    search = Search <$> strArgument (metavar "TEXT") <*> optional limit
    noteid = DocId <$> strArgument (metavar "ID" <> help "note id")
    text = strArgument $ metavar "TEXT" <> help "note text"
    end = dateOption $ long "end" <> short 'e' <> help "end date"
    limit = option auto $ long "limit" <> short 'l' <> help "Number of issues"
    start = dateOption $ long "start" <> short 's' <> help "start date"
    textOption = strOption $
        long "text" <> short 't' <> help "note text" <> metavar "TEXT"
    maybeEnd =
        Just <$> end <|>
        flag' Nothing (long "end-clear" <> help "clear end date")

    dateOption m = option auto $ metavar "DATE" <> m

    cmdConfig = fmap CmdConfig . optional $
        subparser $ command "dataDir" iDataDir <> command "ui" iUi
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIR" <> help "path")
            pYandexDisk = flag'
                DataDirYandexDisk
                (long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk")
        iUi = i ui "UI tweaks"
        ui = fmap ConfigUI . optional $
            flag'
                Shuffle
                (long "shuffle" <> help "shuffle notes in section") <|>
            flag' Sort (long "sort" <> help "sort notes in section")

    version = flag'
        CmdVersion
        (long "version" <> short 'V' <> help "Current ff-note version")

    i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc
