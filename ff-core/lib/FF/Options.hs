{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE OverloadedLists #-}
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
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (Mod, auto, command, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, short,
                                      strArgument, strOption, subparser, value,
                                      (<**>))

import           FF.Storage (DocId (DocId))
import           FF.Types (NoteId)
import           GHC.Exts (IsList, Item, fromList, toList)

data Cmd
    = CmdConfig (Maybe Config)
    | CmdAction CmdAction
    | CmdOption
    | CmdGithubIssue

data CmdAction
    = CmdAgenda        Limit
    | CmdDelete        NoteId
    | CmdDone          NoteId
    | CmdEdit          Edit
    | CmdNew           New
    | CmdPostpone      NoteId
    | CmdSearch        Search
    | CmdUnarchive     NoteId
    -- | CmdGithubIssue

-- data List = List

type Limit = Int

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

data Search = Search Text Limit

parseOptions :: IO Cmd
parseOptions = execParser $ i parser "A note taker and task tracker"
  where
    parser   = subparser commands <|> pCmdAgenda <|> pCmdOption <|> pCmdGithubIssue
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
        , command "unarchive" iCmdUnarchive
        -- , command "github"    iCmdGithubIssue
        ]

    iCmdAdd         = i pCmdNew       "add a new task or note"
    iCmdAgenda      = i pCmdAgenda    "show what you can do right now\
                                    \ [default action]"
    iCmdConfig      = i pCmdConfig    "show/edit configuration"
    iCmdDelete      = i pCmdDelete    "delete a task"
    iCmdDone        = i pCmdDone      "mark a task done (archive)"
    iCmdEdit        = i pCmdEdit      "edit a task or a note"
    iCmdNew         = i pCmdNew       "synonym for `add`"
    iCmdPostpone    = i pCmdPostpone  "make a task start later"
    iCmdSearch      = i pCmdSearch    "search for notes with the given text"
    iCmdUnarchive   = i pCmdUnarchive "restore the note from archive"
    -- iCmdGithubIssue = i pCmdGithubIssue "list issues from github"

    pCmdAgenda      = CmdAction . CmdAgenda         <$> limitOption
    pCmdDelete      = CmdAction . CmdDelete         <$> idArgument
    pCmdDone        = CmdAction . CmdDone           <$> idArgument
    pCmdEdit        = CmdAction . CmdEdit           <$> pEdit
    pCmdNew         = CmdAction . CmdNew            <$> pNew
    pCmdPostpone    = CmdAction . CmdPostpone       <$> idArgument
    pCmdSearch      = CmdAction . CmdSearch         <$> pSearch
    pCmdUnarchive   = CmdAction . CmdUnarchive      <$> idArgument
    -- pCmdGithubIssue = pure $ CmdAction CmdGithubIssue

    -- list = flag' List [long "list", help "list github issues"]
    pCmdGithubIssue = flag' CmdGithubIssue [long "github", help "list github issues"]

    pNew = New <$> textArgument <*> optional startOption <*> optional endOption
    pEdit = Edit
        <$> idArgument
        <*> optional textOption
        <*> optional startOption
        <*> optional maybeEndOption

    pSearch      = Search <$> strArgument (metavar "TEXT") <*> limitOption

    idArgument   = DocId <$> strArgument [metavar "ID", help "note id"]
    textArgument = strArgument [metavar "TEXT", help "note text"]

    endOption    = dateOption [long "end", short 'e', help "end date"]
    limitOption  = option auto [long "limit", short 'l', help "limit", value 10]
    startOption  = dateOption [long "start", short 's', help "start date"]
    textOption   = strOption
                   [long "text", short 't', help "note text", metavar "TEXT"]

    maybeEndOption =
        Just <$> endOption <|>
        flag' Nothing [long "end-clear", help "clear end date"]

    dateOption m = option auto [metavar "DATE", m]

    pCmdConfig = fmap CmdConfig . optional $
        subparser [command "dataDir" iDataDir, command "ui" iUi]
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument [metavar "DIR", help "path"]
            pYandexDisk = flag'
                DataDirYandexDisk
                [long "yandex-disk", short 'y', help "detect Yandex.Disk"]
        iUi = i pUi "UI tweaks"
        pUi = fmap ConfigUI . optional $
            flag' Shuffle [long "shuffle", help "shuffle notes in section"] <|>
            flag' Sort [long "sort", help "sort notes in section"]

    pCmdOption = flag'
        CmdOption [long "version", short 'V', help "Current ff-note version"]

    i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc


instance IsList (Mod f a) where
    type Item (Mod f a) = Mod f a
    fromList = mconcat
    toList = pure
