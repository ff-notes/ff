{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- global
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import           Control.Monad (void)
import           Data.Foldable (for_)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import           Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           System.Environment (getArgs, getProgName)
import           System.IO (hPutStrLn, stderr)

-- organization
import           RON.Storage.Backend (CollectionName, DocId (DocId), RawDocId,
                                      collectionName)
import           RON.Storage.FS (runStorage)
import qualified RON.Storage.FS as Storage

-- project
import           FF (filterTasksByStatus, getDataDir, loadAllNotes,
                     noDataDirectoryMessage, viewNote)
import           FF.Config (loadConfig)
import           FF.Types (Note, Status (Active), loadNote)

-- package
import           FF.Qt (whenUIIdle)
import           FF.Qt.MainWindow (MainWindow)
import qualified FF.Qt.MainWindow as MainWindow
import qualified Paths_ff_qtah as PackageInfo

main :: IO ()
main = do
  progName <- getProgName
  dataDir <- getDataDirOrFail
  storage <- Storage.newHandle dataDir
  changedDocs <- Storage.subscribe storage
  withApp $ \_ -> do
    setupApp
    window <- MainWindow.new progName storage
    MainWindow.show window
    initializeAsync storage window
    whenUIIdle $ checkDBChange storage window changedDocs
    QCoreApplication.exec

withApp :: (QApplication -> IO a) -> IO a
withApp action = do
  args <- getArgs
  withScopedPtr (QApplication.new args) action

setupApp :: IO ()
setupApp = do
  QCoreApplication.setOrganizationDomain "ff.cblp.su"
  QCoreApplication.setOrganizationName "ff"
  QCoreApplication.setApplicationName "ff"
  QCoreApplication.setApplicationVersion $ showVersion PackageInfo.version

initializeAsync :: Storage.Handle -> MainWindow -> IO ()
initializeAsync storage window =
  void $ forkIO $ do
    activeTasks <- runStorage storage $ do
      notes <- loadAllNotes
      let activeTaskEntities = filterTasksByStatus Active notes
      traverse viewNote activeTaskEntities
    for_ activeTasks $ MainWindow.upsertNote window

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
  dataDir <- getDataDir cfg
  case dataDir of
    Nothing -> fail noDataDirectoryMessage
    Just path -> pure path

upsertDocument ::
  Storage.Handle -> MainWindow -> CollectionName -> RawDocId -> IO ()
upsertDocument storage window collection docid
  | collection == collectionName @Note = do
      note <- runStorage storage $ loadNote (DocId docid) >>= viewNote
      MainWindow.upsertNote window note
  | otherwise =
      hPutStrLn stderr $ "upsertDocument: unknown document type " <> show docid
