{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (getRealLocalTime)
import           Data.Foldable (traverse_)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QAbstractItemView (setAlternatingRowColors, setCurrentIndex,
                                    setItemDelegate, setModel)
import           QApplication (QApplication, new)
import           QCloseEvent (QCloseEvent)
import           QCoreApplication (exec, setApplicationName,
                                   setApplicationVersion, setOrganizationDomain,
                                   setOrganizationName)
import           QMainWindow (QMainWindow, new, restoreState, saveState,
                              setCentralWidget)
import           QSettings (new, setValue, value)
import           QShowEvent (QShowEvent)
import           QStandardItem (index)
import           QStandardItemModel (item)
import           QStyledItemDelegate (new)
import           QTabWidget (QTabWidget, addTab, new)
import           Qtah.Event (onEvent)
import           QTreeView (QTreeView, expandAll, new, setHeaderHidden)
import           QVariant (newWithByteArray, toByteArray)
import           QWidget (QWidgetPtr, restoreGeometry, saveGeometry, setFocus,
                          setWindowTitle, show)
import           System.Environment (getArgs)

import           FF (loadActiveNotes)
import           FF.Config (Config (Config, dataDir), loadConfig)
import           FF.Storage (runStorage)

import           NoteModel (NoteModel (super), addNote, new)
import           Paths_ff_qt (version)

main :: IO ()
main = do
    Config { dataDir = Just dataDir } <- loadConfig
    timeVar                           <- newTVarIO =<< getRealLocalTime
    withApp $ \app -> do
        setOrganizationDomain app "ff.cblp.su"
        setOrganizationName   app "ff"
        setApplicationName    app "ff"
        setApplicationVersion app $ showVersion version

        model <- NoteModel.new
        runStorage dataDir timeVar loadActiveNotes >>= traverse_ (addNote model)

        mainWindow <- mkMainWindow model
        QWidget.show mainWindow
        exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

mkMainWindow :: NoteModel -> IO QMainWindow
mkMainWindow model = do
    this <- QMainWindow.new
    setCentralWidget this =<< do
        tabs <- QTabWidget.new
        addTab_ tabs "Agenda" =<< mkAgendaWidget model
        pure tabs
    setWindowTitle this "ff"

    -- https://wiki.qt.io/Saving_Window_Size_State
    void $ onEvent this $ \(_ :: QShowEvent) -> do
        withScopedPtr QSettings.new $ \settings -> do
            void
                $   value settings "mainWindowGeometry"
                >>= toByteArray
                >>= restoreGeometry this
            void
                $   value settings "mainWindowState"
                >>= toByteArray
                >>= restoreState this
        pure False
    void $ onEvent this $ \(_ :: QCloseEvent) -> do
        withScopedPtr QSettings.new $ \settings -> do
            setValue settings "mainWindowGeometry"
                =<< QVariant.newWithByteArray
                =<< saveGeometry this
            setValue settings "mainWindowState"
                =<< QVariant.newWithByteArray
                =<< saveState this
        pure False

    pure this

addTab_ :: QWidgetPtr widget => QTabWidget -> String -> widget -> IO ()
addTab_ tabs name widget = void $ addTab tabs widget name

mkAgendaWidget :: NoteModel -> IO QTreeView
mkAgendaWidget model = do
    this <- QTreeView.new
    setAlternatingRowColors this True
    setHeaderHidden         this True
    setItemDelegate this =<< QStyledItemDelegate.new
    setModel this (super model)
    void $ onEvent this $ \(_ :: QShowEvent) -> do
        setFocus this
        setCurrentIndex this =<< index =<< item (super model) 0
        pure False
    expandAll this

    pure this
