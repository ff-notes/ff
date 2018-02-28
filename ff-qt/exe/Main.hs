{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (LocalTime, getRealLocalTime)
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import           Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow,
                                                       setCentralWidget)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import           Graphics.UI.Qtah.Widgets.QTabWidget (QTabWidget, addTab)
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           Graphics.UI.Qtah.Widgets.QWidget (QWidgetPtr)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

import           FF (loadActiveNotes)
import           FF.Config (Config (Config, dataDir), loadConfig)
import           FF.Storage (runStorage)
import           FF.Types (NoteView (NoteView, text))

main :: IO ()
main = do
    Config { dataDir = Just dataDir } <- loadConfig
    timeVar                           <- newTVarIO =<< getRealLocalTime
    withApp $ \_ -> do
        mainWindow <- mkMainWindow dataDir timeVar
        QWidget.show mainWindow
        exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

mkMainWindow :: FilePath -> TVar LocalTime -> IO QMainWindow
mkMainWindow dataDir timeVar = do
    mainWindow <- QMainWindow.new
    setCentralWidget mainWindow =<< do
        tabs <- QTabWidget.new
        addTab_ tabs "Agenda" =<< mkAgendaWidget dataDir timeVar
        pure tabs
    pure mainWindow

addTab_ :: QWidgetPtr widget => QTabWidget -> String -> widget -> IO ()
addTab_ tabs name widget = void $ addTab tabs widget name

mkAgendaWidget :: FilePath -> TVar LocalTime -> IO QTreeWidget
mkAgendaWidget dataDir timeVar = do
    tree <- QTreeWidget.new
    setHeaderHidden tree True
    notes <- runStorage dataDir timeVar loadActiveNotes
    for_ notes $ \NoteView { text } ->
        QTreeWidgetItem.newWithParentTreeAndStrings tree [Text.unpack text]
    pure tree
