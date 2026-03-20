{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- global
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Version (showVersion)
import Foreign.Hoppy.Runtime (withScopedPtr)
import Graphics.UI.Qtah.Core.QCoreApplication qualified as QCoreApplication
import Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import Graphics.UI.Qtah.Widgets.QApplication qualified as QApplication
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

-- organization
import RON.Storage.Backend (
    CollectionName,
    DocId (DocId),
    RawDocId,
    collectionName,
 )
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as Storage

-- project
import FF (filterTasksByStatus, loadAllNotes, noDataDirectoryMessage, viewNote)
import FF.Config (loadConfig)
import FF.Config qualified
import FF.Types (Note, Status (Active), loadNote)

-- package
import FF.Qt (repeatInGuiThreadWheneverIdle, runInGuiThreadWhenReady)
import FF.Qt.MainWindow (MainWindow)
import FF.Qt.MainWindow qualified as MainWindow
import Paths_ff_qtah qualified as PackageInfo

main :: IO ()
main = do
    progName <- getProgName
    dataDir <- getDataDirOrFail
    storage <- Storage.newHandle dataDir
    changedDocs <- Storage.subscribe storage
    withApp \_ -> do
        setupApp
        window <- MainWindow.new progName storage
        QWidget.show window.parent
        initializeAsync storage window
        repeatInGuiThreadWheneverIdle $ checkDBChange storage window changedDocs
        QCoreApplication.exec

withApp :: (QApplication -> IO a) -> IO a
withApp action = do
    prog <- getProgName
    args <- getArgs
    -- let args'
    --         | any ("-style" `isPrefixOf`) args = args
    --         | otherwise = "-style=Fusion" : args
    withScopedPtr (QApplication.new $ prog : args) action

setupApp :: IO ()
setupApp = do
    QCoreApplication.setOrganizationDomain "ff.cblp.su"
    QCoreApplication.setOrganizationName "ff"
    QCoreApplication.setApplicationName "ff"
    QCoreApplication.setApplicationVersion $ showVersion PackageInfo.version

initializeAsync :: Storage.Handle -> MainWindow -> IO ()
initializeAsync storage window = do
    tasksVar <- newEmptyMVar
    void . forkIO $ do
        activeTasks <-
            runStorage storage do
                notes <- loadAllNotes
                let activeTaskEntities = filterTasksByStatus Active notes
                traverse viewNote activeTaskEntities
        putMVar tasksVar activeTasks
    runInGuiThreadWhenReady tasksVar . traverse_ $ MainWindow.upsertNote window

checkDBChange ::
    Storage.Handle -> MainWindow -> TChan (CollectionName, RawDocId) -> IO ()
checkDBChange storage window changedDocs =
    atomically (tryReadTChan changedDocs) >>= \case
        Just (collection, docid) ->
            upsertDocument storage window collection docid
        Nothing ->
            pure ()

getDataDirOrFail :: IO FilePath
getDataDirOrFail = do
    cfg <- loadConfig
    case cfg.dataDir of
        Nothing -> fail noDataDirectoryMessage
        Just path -> pure path

upsertDocument ::
    Storage.Handle -> MainWindow -> CollectionName -> RawDocId -> IO ()
upsertDocument storage window collection docid
    | collection == collectionName @Note = do
        note <- runStorage storage $ loadNote (DocId docid) >>= viewNote
        MainWindow.upsertNote window note
    | otherwise =
        hPutStrLn stderr $
            "upsertDocument: unknown document type " <> show docid
