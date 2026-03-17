{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Qt.MainWindow (
    MainWindow (parent),
    new,
    upsertNote,
) where

-- global
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Traversable (for)
import Foreign.Hoppy.Runtime (withScopedPtr)
import GHC.Stack (callStack, prettyCallStack)
import Graphics.UI.Qtah.Core.QSettings qualified as QSettings
import Graphics.UI.Qtah.Core.QVariant qualified as QVariant
import Graphics.UI.Qtah.Event (onEvent)
import Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QAction qualified as QAction
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow, QMainWindowPtr)
import Graphics.UI.Qtah.Widgets.QMainWindow qualified as QMainWindow
import Graphics.UI.Qtah.Widgets.QMenu qualified as QMenu
import Graphics.UI.Qtah.Widgets.QMenuBar qualified as QMenuBar
import Graphics.UI.Qtah.Widgets.QMessageBox qualified as QMessageBox
import Graphics.UI.Qtah.Widgets.QSplitter qualified as QSplitter
import Graphics.UI.Qtah.Widgets.QTreeWidget qualified as QTreeWidget
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem qualified as QTreeWidgetItem
import Graphics.UI.Qtah.Widgets.QWidget (QWidgetPtr)
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import System.IO (hPutStrLn, stderr)

-- organization
import RON.Storage.Backend (DocId (DocId))
import RON.Storage.FS qualified as Storage

-- project
import FF.Types (EntityView, Note)

-- package
import FF.Qt.TaskListWidget (
    ItemType (ModeGroup, Task),
    TaskListWidget,
    itemTypeFromInt,
 )
import FF.Qt.TaskListWidget qualified as TaskListWidget
import FF.Qt.TaskWidget (TaskWidget)
import FF.Qt.TaskWidget qualified as TaskWidget

data MainWindow = MainWindow
    { parent :: QMainWindow
    , agendaTasks :: TaskListWidget
    , taskWidget :: TaskWidget
    }

new :: String -> Storage.Handle -> IO MainWindow
new progName storage = do
    parent <- QMainWindow.new
    QWidget.setWindowTitle parent progName

    restoreGeometry parent -- must be before widgets creation

    -- UI setup and widgets creation
    agendaSplitter <- QSplitter.new
    QSplitter.setChildrenCollapsible agendaSplitter False
    QMainWindow.setCentralWidget parent agendaSplitter

    agendaTasks <- TaskListWidget.new
    QSplitter.addWidget agendaSplitter agendaTasks.parent

    taskWidget <- TaskWidget.new storage
    QWidget.hide taskWidget.parent
    QSplitter.addWidget agendaSplitter taskWidget.parent

    -- sizes need widgets to be added
    QSplitter.setSizes agendaSplitter [1, 1 :: Int]

    do
        menuBar <- QMainWindow.menuBar parent
        do
            debugMenu <- QMenuBar.addNewMenu menuBar "&Debug"
            showUuidsAction <-
                QMenu.addNewAction debugMenu "&Show UUIDs and internal keys"
            QAction.setCheckable showUuidsAction True
            connect_ showUuidsAction QAction.toggledSignal $
                TaskListWidget.setDebugInfoVisible agendaTasks
        do
            helpMenu <- QMenuBar.addNewMenu menuBar "&Help"
            aboutProgramAction <- QMenu.addNewAction helpMenu "&About ff"
            connect_ aboutProgramAction QAction.triggeredSignal $
                const $
                    showAboutProgram parent progName

    restoreState parent -- must be after widgets creation
    let mainWindow = MainWindow{parent, agendaTasks, taskWidget}

    -- handling events
    void $ onEvent parent \(_ :: QCloseEvent) -> saveGeometryAndState parent
    -- TODO
    -- connect_ editor QTextEdit.textChangedSignal $ saveTheText storage editor
    connect_ agendaTasks.parent QTreeWidget.itemSelectionChangedSignal $
        resetTaskView mainWindow

    pure mainWindow

-- | Only task notes are supported. TODO support wiki notes too
upsertNote :: MainWindow -> EntityView Note -> IO ()
upsertNote MainWindow{agendaTasks} = TaskListWidget.upsertTask agendaTasks

-- https://wiki.qt.io/Saving_Window_Size_State
saveGeometryAndState :: (QMainWindowPtr window) => window -> IO Bool
saveGeometryAndState window =
    withScopedPtr QSettings.new \settings -> do
        let saveSetting name value =
                QVariant.newWithByteArray value
                    >>= QSettings.setValue settings name
        QWidget.saveGeometry window >>= saveSetting "mainWindowGeometry"
        QMainWindow.saveState window >>= saveSetting "mainWindowState"
        pure True

restoreGeometry :: (QWidgetPtr widget) => widget -> IO ()
restoreGeometry widget =
    void $ loadSetting "mainWindowGeometry" >>= QWidget.restoreGeometry widget

restoreState :: (QMainWindowPtr window) => window -> IO ()
restoreState window =
    void $ loadSetting "mainWindowState" >>= QMainWindow.restoreState window

loadSetting :: String -> IO ByteString
loadSetting name =
    withScopedPtr QSettings.new \settings ->
        QSettings.value settings name >>= QVariant.toByteArray

resetTaskView :: MainWindow -> IO ()
resetTaskView MainWindow{agendaTasks, taskWidget} = do
    items <- QTreeWidget.selectedItems agendaTasks.parent
    taskItems <-
        fold <$> for items \item -> do
            itemType <- itemTypeFromInt <$> QTreeWidgetItem.getType item
            pure case itemType of
                Task -> [item]
                ModeGroup -> []
    case taskItems of
        [] -> QWidget.hide taskWidget.parent
        [item] -> setTaskView taskWidget item
        _ : _ : _ -> print "TODO open/replace group actions view"

setTaskView :: TaskWidget -> QTreeWidgetItem -> IO ()
setTaskView taskWidget item = do
    itemType <- itemTypeFromInt <$> QTreeWidgetItem.getType item
    case itemType of
        ModeGroup ->
            hPutStrLn stderr $ "internal error" ++ prettyCallStack callStack
        Task -> do
            noteId <- DocId @Note <$> TaskListWidget.getId item
            TaskWidget.reload taskWidget noteId
            QWidget.show taskWidget.parent

showAboutProgram :: (QWidgetPtr mainWindow) => mainWindow -> String -> IO ()
showAboutProgram mainWindow progName =
    QMessageBox.about mainWindow progName "A note taker and task tracker"
