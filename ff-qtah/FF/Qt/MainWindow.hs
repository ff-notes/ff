{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Qt.MainWindow (
  MainWindow, new, FF.Qt.MainWindow.show, upsertNote
) where

-- global
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QSettings as QSettings
import qualified Graphics.UI.Qtah.Core.QVariant as QVariant
import           Graphics.UI.Qtah.Event (onEvent)
import           Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import           Graphics.UI.Qtah.Signal (connect_)
import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow,
                                                       QMainWindowPtr)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import           Graphics.UI.Qtah.Widgets.QWidget (QWidgetPtr)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           RON.Storage.Backend (DocId (DocId))
import qualified RON.Storage.FS as Storage

-- project
import           FF.Types (EntityView, Note)

-- package
import           FF.Qt.TaskListWidget (TaskListWidget)
import qualified FF.Qt.TaskListWidget as TaskListWidget
import           FF.Qt.TaskWidget (TaskWidget)
import qualified FF.Qt.TaskWidget as TaskWidget

data MainWindow = MainWindow
  { window      :: QMainWindow
  , agendaTasks :: TaskListWidget
  , taskWidget  :: TaskWidget
  }

new :: String -> Storage.Handle -> IO MainWindow
new progName storage = do
  window <- QMainWindow.new
  restoreGeometry window -- must be before widgets creation

  -- UI setup and widgets creation
  agendaSplitter <- QSplitter.new
  QSplitter.setChildrenCollapsible agendaSplitter False
  QMainWindow.setCentralWidget window agendaSplitter

  QWidget.setWindowTitle window progName
  agendaTasks <- TaskListWidget.new
  QSplitter.addWidget agendaSplitter agendaTasks

  taskWidget <- TaskWidget.new storage
  QWidget.hide taskWidget
  QSplitter.addWidget agendaSplitter taskWidget

  restoreState window -- must be after widgets creation

  let mainWindow = MainWindow{window, agendaTasks, taskWidget}

  -- handling events
  void $ onEvent window $ \(_ :: QCloseEvent) -> saveGeometryAndState window
  -- TODO
  -- connect_ editor QTextEdit.textChangedSignal $ saveTheText storage editor
  connect_ agendaTasks QTreeWidget.itemSelectionChangedSignal $
    resetTaskView mainWindow

  pure mainWindow

show :: MainWindow -> IO ()
show MainWindow{window} = QWidget.show window

-- | Only task notes are supported. TODO support wiki notes too
upsertNote :: MainWindow -> EntityView Note -> IO ()
upsertNote MainWindow{agendaTasks} = TaskListWidget.upsertTask agendaTasks

-- https://wiki.qt.io/Saving_Window_Size_State
saveGeometryAndState :: QMainWindowPtr window => window -> IO Bool
saveGeometryAndState window =
  withScopedPtr QSettings.new $ \settings -> do
    let saveSetting name value =
          QVariant.newWithByteArray value >>= QSettings.setValue settings name
    QWidget.saveGeometry  window >>= saveSetting "mainWindowGeometry"
    QMainWindow.saveState window >>= saveSetting "mainWindowState"
    pure True

restoreGeometry :: QWidgetPtr widget => widget -> IO ()
restoreGeometry widget =
  void $ loadSetting "mainWindowGeometry" >>= QWidget.restoreGeometry widget

restoreState :: QMainWindowPtr window => window -> IO ()
restoreState window =
  void $ loadSetting "mainWindowState" >>= QMainWindow.restoreState window

loadSetting :: String -> IO ByteString
loadSetting name =
  withScopedPtr QSettings.new $ \settings ->
    QSettings.value settings name >>= QVariant.toByteArray

resetTaskView :: MainWindow -> IO ()
resetTaskView MainWindow{agendaTasks, taskWidget} = do
  items <- QTreeWidget.selectedItems agendaTasks
  case items of
    []     -> QWidget.hide taskWidget
    [item] -> setTaskView taskWidget item
    _:_:_  -> print "TODO open/replace group actions view"

setTaskView :: TaskWidget -> QTreeWidgetItem -> IO ()
setTaskView taskWidget item = do
  noteId <- DocId @Note <$> TaskListWidget.getNoteId item
  TaskWidget.update taskWidget noteId
  QWidget.show taskWidget
