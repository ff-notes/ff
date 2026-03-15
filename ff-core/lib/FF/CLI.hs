{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module FF.CLI where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (forever, guard, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as JSON
import Data.Aeson.Encode.Pretty qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Foldable (asum, for_, toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text, snoc)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as TextError
import Data.Text.IO qualified as Text
import Data.Time (Day)
import Data.Traversable (for)
import Data.Version (Version, showVersion)
import Development.GitRev (gitDirty, gitHash)
import Prettyprinter (
    Doc,
    PageWidth (AvailablePerLine),
    defaultLayoutOptions,
    layoutPageWidth,
    layoutPretty,
    pretty,
    vsep,
 )
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import RON.Storage.Backend (MonadStorage)
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as StorageFS
import ShellWords qualified
import System.Console.Terminal.Size qualified as Terminal
import System.Directory (doesDirectoryExist, findExecutable, getHomeDirectory)
import System.Environment (getEnv, lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hClose, hPutChar, hPutStr, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Pager (printOrPage)
import System.Process.Typed (proc, runProcess)

import FF (
    cmdAddTagToGroup,
    cmdDeleteContact,
    cmdDeleteNote,
    cmdDone,
    cmdEdit,
    cmdNewContact,
    cmdNewNote,
    cmdPostpone,
    cmdSearch,
    cmdUnarchive,
    defaultNoteFilter,
    filterTasksByStatus,
    getContactSamples,
    getUtcToday,
    groupByTagGroup,
    loadAllNotes,
    loadAllTagTexts,
    loadAllTags,
    noDataDirectoryMessage,
    sponsors,
    updateTrackedNotes,
    viewNote,
    viewTaskSamples,
    viewWikiSamples,
 )
import FF qualified
import FF.Config (Config (..), ConfigUI (..), appName, loadConfig, saveConfig)
import FF.Github (getIssueViews, getOpenIssueSamples)
import FF.Options (
    ActionOptions (..),
    Agenda (..),
    Cmd (..),
    CmdAction (..),
    Contact (..),
    DataDir (..),
    Options (..),
    Search (..),
    Shuffle (..),
    TagsRequest (EmptyTagsRequest),
    Track (..),
    parseOptions,
 )
import FF.Options qualified as Options
import FF.Types (Status (Active), loadNote)
import FF.Types qualified as Sample (Sample (items))
import FF.UI (
    prettyContact,
    prettyContactSample,
    prettyNote,
    prettyNoteList,
    prettyPath,
    prettyTagsList,
    prettyTaskSections,
    prettyTasksWikisContacts,
    prettyWikiSample,
    withHeader,
    (<//>),
 )
import FF.Upgrade (upgradeDatabase)

cli :: Version -> IO ()
cli version = do
    baseCfg@Config{dataDir} <- loadConfig
    handle' <- traverse StorageFS.newHandle dataDir
    Options{customDir, cmd, actionOptions} <- parseOptions handle'
    let ActionOptions{json} = actionOptions
    (handle, cfg) <-
        case customDir of
            Nothing -> pure (handle', baseCfg)
            Just path -> do
                h <- StorageFS.newHandle path
                pure (Just h, baseCfg{dataDir = customDir})
    case cmd of
        CmdConfig param -> runCmdConfig baseCfg param
        CmdVersion -> runCmdVersion json version
        CmdAction action -> case handle of
            Nothing -> fail noDataDirectoryMessage
            Just h -> runStorage h $ runCmdAction cfg action actionOptions

runCmdConfig :: Config -> Maybe Options.Config -> IO ()
runCmdConfig cfg@Config{dataDir, externalEditor, ui} = \case
    Nothing -> jprint cfg
    Just (Options.ConfigDataDir mDir) -> do
        dir <-
            case mDir of
                Nothing -> pure dataDir
                Just (DataDirJust dir) -> saveDataDir dir
                Just DataDirYandexDisk -> do
                    home <- getHomeDirectory
                    asum
                        [ trySaveDataDir $ home </> "Yandex.Disk"
                        , trySaveDataDir $ home </> "Yandex.Disk.localized"
                        , fail "Cant't detect Yandex.Disk directory"
                        ]
        jprint dir
    Just (Options.ConfigExternalEditor mPath) -> do
        path <-
            case mPath of
                Nothing -> pure externalEditor
                Just path -> saveExternalEditor path
        jprint path
    Just (Options.ConfigUI mShuffle) -> do
        ui' <-
            case mShuffle of
                Nothing -> pure ui
                Just Shuffle -> saveShuffle True
                Just Sort -> saveShuffle False
        jprint ui'
  where
    trySaveDataDir baseDir = do
        guard =<< doesDirectoryExist baseDir
        saveDataDir $ baseDir </> "Apps" </> appName
    saveDataDir dir = saveConfig cfg{dataDir = Just dir} $> Just dir
    saveExternalEditor path =
        saveConfig cfg{externalEditor = Just path} $> Just path
    saveShuffle shuffle' = saveConfig cfg{ui = ui'} $> ui'
      where
        ui' = ConfigUI{shuffle = shuffle'}

runCmdAction ::
    (MonadIO m, MonadStorage m) =>
    Config ->
    CmdAction ->
    ActionOptions ->
    m ()
runCmdAction cfg cmd options@ActionOptions{brief, json} = do
    today <- getUtcToday
    case cmd of
        CmdAgenda a -> runCmdAgenda cfg a today options
        CmdContact contact -> cmdContact options cfg.dataDir contact
        CmdDelete noteIds -> do
            notes <-
                for noteIds $ \noteId -> do
                    note <- cmdDeleteNote noteId
                    viewNote note
            if json then
                jprintObject
                    [ "result" .= ("deleted" :: Text)
                    , "notes" .= notes
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    withHeader "Deleted:" (prettyNoteList brief $ toList notes)
                        <//> prettyPath cfg.dataDir
        CmdDone noteIds -> do
            notes <-
                for noteIds $ \noteId -> do
                    note <- cmdDone noteId
                    viewNote note
            if json then
                jprintObject
                    [ "result" .= ("archived" :: Text)
                    , "notes" .= notes
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    withHeader "Archived:" (prettyNoteList brief $ toList notes)
                        <//> prettyPath cfg.dataDir
        CmdEdit edit -> do
            notes <-
                cmdEdit edit $
                    if json then
                        const $ liftIO Text.getContents
                    else
                        liftIO . runExternalEditor
            notes' <- traverse viewNote notes
            if json then
                jprintObject
                    [ "result" .= ("edited" :: Text)
                    , "notes" .= notes'
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    withHeader "Edited:" (prettyNoteList brief notes')
                        <//> prettyPath cfg.dataDir
        CmdNew new -> do
            note <- cmdNewNote new today
            noteview <- viewNote note
            if json then
                jprintObject
                    [ "result" .= ("added" :: Text)
                    , "note" .= noteview
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    withHeader "Added:" (prettyNote brief noteview)
                        <//> prettyPath cfg.dataDir
        CmdPostpone noteIds -> do
            notes <-
                for noteIds $ \noteId -> do
                    note <- cmdPostpone noteId
                    viewNote note
            if json then
                jprintObject
                    [ "result" .= ("postponed" :: Text)
                    , "notes" .= notes
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    withHeader
                        "Postponed:"
                        (prettyNoteList brief $ toList notes)
                        <//> prettyPath cfg.dataDir
        CmdSearch Search{..} -> do
            (tasks, wikis, contacts) <-
                cmdSearch text status cfg.ui limit today tags
            if json then
                jprintObject
                    [ "tasks" .= foldMap (.items) tasks
                    , "wiki" .= wikis
                    , "contacts" .= contacts
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    prettyTasksWikisContacts
                        brief
                        tasks
                        wikis
                        contacts
                        inTasks
                        inWikis
                        inContacts
                        tags
                        <//> prettyPath cfg.dataDir
        CmdShow noteIds -> do
            notes <- for noteIds loadNote
            notes' <- traverse viewNote notes
            if json then
                jprintObject ["notes" .= notes', "database" .= cfg.dataDir]
            else
                pprint $
                    prettyNoteList brief (toList notes')
                        <//> prettyPath cfg.dataDir
        CmdTag{tag, group} -> do
            _ <- cmdAddTagToGroup tag group
            if json then
                jprintObject
                    ["result" .= ("added" :: Text), "database" .= cfg.dataDir]
            else
                pprint $
                    withHeader
                        "Added tag to group:"
                        (pretty tag <> " → " <> pretty group)
                        <//> prettyPath cfg.dataDir
        CmdTags
            | json -> do
                tags <- loadAllTags
                jprintObject ["tags" .= tags, "database" .= cfg.dataDir]
            | otherwise -> do
                tags <- loadAllTagTexts
                pprint $ prettyTagsList tags <//> prettyPath cfg.dataDir
        CmdSponsors
            | json -> jprintObject ["sponsors" .= sponsors]
            | otherwise ->
                pprint $ withHeader "Sponsors" $ vsep $ map pretty sponsors
        CmdTrack track -> cmdTrack track today brief
        CmdUnarchive tasks ->
            for_ tasks $ \taskId -> do
                task <- cmdUnarchive taskId
                noteview <- viewNote task
                if json then
                    jprintObject
                        [ "result" .= ("unarchived" :: Text)
                        , "note" .= noteview
                        , "database" .= cfg.dataDir
                        ]
                else
                    pprint $
                        withHeader "Unarchived:" (prettyNote brief noteview)
                            <//> prettyPath cfg.dataDir
        CmdUpgrade -> do
            upgradeDatabase
            if json then
                jprintObject
                    [ "result" .= ("database upgraded" :: Text)
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $ "Database upgraded" <//> prettyPath cfg.dataDir
        CmdWiki mlimit -> do
            notes <- loadAllNotes
            wikis <- viewWikiSamples cfg.ui mlimit today notes
            if json then
                jprintObject ["wiki" .= wikis, "database" .= cfg.dataDir]
            else
                pprint $
                    prettyWikiSample brief wikis <//> prettyPath cfg.dataDir

cmdTrack :: (MonadIO m, MonadStorage m) => Track -> Day -> Bool -> m ()
cmdTrack Track{dryRun, address, limit} today brief
    | dryRun =
        liftIO do
            samples <- run $ getOpenIssueSamples address limit today
            pprint $ prettyTaskSections brief EmptyTagsRequest samples
    | otherwise = do
        notes <- liftIO $ run $ getIssueViews address limit
        updateTrackedNotes notes
        liftIO $
            putStrLn $
                show (length notes) ++ " issues synchronized with the local database"
  where
    run getter = do
        hPutStr stderr "fetching"
        eIssues <-
            fromEither
                <$> race
                    (runExceptT getter)
                    (forever $ hPutChar stderr '.' >> threadDelay 500000)
        hPutStrLn stderr ""
        case eIssues of
            Left err -> do
                Text.hPutStrLn stderr err
                exitFailure
            Right issues -> pure issues

cmdContact ::
    (MonadIO m, MonadStorage m) =>
    ActionOptions ->
    Maybe FilePath ->
    Maybe Contact ->
    m ()
cmdContact ActionOptions{json, brief} path = \case
    Just (Add name) -> do
        contact <- cmdNewContact name
        if json then
            jprintObject
                [ "result" .= ("added" :: Text)
                , "contact" .= contact
                , "database" .= path
                ]
        else
            pprint $
                withHeader "Added:" (prettyContact brief contact)
                    <//> prettyPath path
    Just (Delete cid) -> do
        contact <- cmdDeleteContact cid
        if json then
            jprintObject
                [ "result" .= ("deleted" :: Text)
                , "contact" .= contact
                , "database" .= path
                ]
        else
            pprint $
                withHeader "Deleted:" (prettyContact brief contact)
                    <//> prettyPath path
    Nothing -> do
        contacts <- getContactSamples Active
        if json then
            jprintObject ["contacts" .= contacts, "database" .= path]
        else
            pprint $ prettyContactSample brief contacts <//> prettyPath path

{- | Template taken from stack:
"Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e8 (5807 commits)"
-}
runCmdVersion :: Bool -> Version -> IO ()
runCmdVersion json version
    | json =
        jprintObject
            [ "app_version" .= version
            , "git_revision" .= ($gitHash :: Text)
            , "git_dirty" .= $gitDirty
            ]
    | otherwise =
        putStrLn $
            concat
                [ "Version "
                , showVersion version
                , ", Git revision "
                , $gitHash
                , if $gitDirty then ", dirty" else ""
                ]

pprint :: (MonadIO io) => Doc AnsiStyle -> io ()
pprint doc = liftIO do
    -- enable colors in `less`
    lessConf <- lookupEnv "LESS"
    when (isNothing lessConf) $ setEnv "LESS" "-R"
    width <- maybe 80 Terminal.width <$> Terminal.size
    let layoutOptions =
            defaultLayoutOptions{layoutPageWidth = AvailablePerLine width 1}
    printOrPage . (`snoc` '\n') . renderStrict $ layoutPretty layoutOptions doc

fromEither :: Either a a -> a
fromEither = either id id

jprint :: (MonadIO io, ToJSON a) => a -> io ()
jprint = liftIO . BSL.putStrLn . JSON.encodePretty

jprintObject :: (MonadIO io) => [JSON.Pair] -> io ()
jprintObject = jprint . JSON.object

runExternalEditor :: Text -> IO Text
runExternalEditor textOld = do
    editor :| editorArgs <-
        asum $
            assertExecutableFromConfig
                : assertExecutableFromEnv "VISUAL"
                : assertExecutableFromEnv "EDITOR"
                : map assertExecutable ["editor", "micro", "nano", "vi", "vim"]
    withSystemTempFile "ff.txt" $ \file fileH -> do
        BS.hPutStr fileH $ Text.encodeUtf8 textOld
        hClose fileH
        hPutStrLn stderr "waiting for external editor to close"
        runProcess (proc editor $ editorArgs ++ [file]) >>= \case
            ExitSuccess ->
                Text.strip . Text.decodeUtf8With TextError.ignore
                    <$> BS.readFile file
            ExitFailure{} -> pure textOld
  where
    assertExecutable prog = do
        Just _ <- findExecutable prog
        pure $ prog :| []

    assertExecutableFromConfig = do
        cfg <- loadConfig
        case externalEditor cfg of
            Nothing -> fail "Empty config parameter 'externalEditor'"
            Just editor ->
                assertExecutableWithArgs
                    "config parameter 'externalEditor'"
                    editor

    assertExecutableFromEnv var = do
        cmd <- getEnv var
        assertExecutableWithArgs ("environment variable " <> var) cmd

    assertExecutableWithArgs source cmd = do
        case ShellWords.parse cmd of
            Left err -> fail $ "bad editor command in " <> source <> ": " <> err
            Right [] -> fail $ "empty editor command in " <> source
            Right (prog : args) -> assertExecutable prog $> prog :| args

runCmdAgenda ::
    (MonadIO m, MonadStorage m) =>
    Config -> Agenda -> Day -> ActionOptions -> m ()
runCmdAgenda cfg agendaCfg today actionOptions = do
    notes <- loadAllNotes
    case agendaCfg.tagGroup of
        Nothing -> do
            samples <-
                viewTaskSamples
                    defaultNoteFilter{FF.tags = agendaCfg.tags}
                    cfg.ui
                    agendaCfg.limit
                    today
                    notes
            if actionOptions.json then
                jprintObject
                    [ "notes" .= foldMap (.items) samples
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    prettyTaskSections
                        actionOptions.brief
                        agendaCfg.tags
                        samples
                        <//> prettyPath cfg.dataDir
        Just tagGroup -> do
            noteGroups <-
                groupByTagGroup tagGroup $ filterTasksByStatus Active notes
            if actionOptions.json then
                jprintObject
                    [ "notes" .= Map.mapKeys tagSetLabel noteGroups
                    , "database" .= cfg.dataDir
                    ]
            else
                pprint $
                    vsep
                        [ withHeader (tagSetLabel tagSet) $
                            prettyNoteList actionOptions.brief noteSample.items
                        | (tagSet, noteSample) <- Map.toAscList noteGroups
                        ]
                        <//> prettyPath cfg.dataDir
  where
    tagSetLabel tagSet
        | null tagSet = "untagged"
        | otherwise = Text.intercalate " + " $ toList tagSet