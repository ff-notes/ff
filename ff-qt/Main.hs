{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (LocalTime, getRealLocalTime)
import           Data.Foldable (traverse_)
import qualified Data.Text as Text
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QApplication (QApplication, new)
import           QBoxLayout (addStretch, insertWidget)
import           QCloseEvent (QCloseEvent)
import           QCoreApplication (exec, setApplicationName,
                                   setApplicationVersion, setOrganizationDomain,
                                   setOrganizationName)
import           QLabel (QLabel, newWithText)
import           QLayout (QLayoutConstPtr, count)
import           QMainWindow (QMainWindow, new, restoreState, saveState,
                              setCentralWidget)
import           QSettings (new, setValue, value)
import           QShowEvent (QShowEvent)
import           QTabWidget (QTabWidget, addTab, new)
import           Qtah.Event (onEvent)
import           QToolBox (QToolBox, addItem, new)
import           QVariant (newWithByteArray, toByteArray)
import           QVBoxLayout (newWithParent)
import           QWidget (QWidgetPtr, new, restoreGeometry, saveGeometry,
                          setWindowTitle, show)
import           System.Environment (getArgs)

import           FF (getUtcToday, loadActiveNotes)
import           FF.Config (Config (Config, dataDir), loadConfig)
import           FF.Storage (runStorage)
import           FF.Types (ModeMap (..), NoteView (NoteView, text), modeSelect,
                           taskMode)

import           Paths_ff_qt (version)

data Storage = Storage FilePath (TVar LocalTime)

main :: IO ()
main = do
    Config { dataDir = Just dataDir } <- loadConfig
    timeVar                           <- newTVarIO =<< getRealLocalTime
    let storage = Storage dataDir timeVar
    withApp $ \app -> do
        setOrganizationDomain app "ff.cblp.su"
        setOrganizationName   app "ff"
        setApplicationName    app "ff"
        setApplicationVersion app $ showVersion version

        mainWindow <- newMainWindow storage
        QWidget.show mainWindow
        exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

newMainWindow :: Storage -> IO QMainWindow
newMainWindow storage = do
    this <- QMainWindow.new
    setCentralWidget this =<< do
        tabs <- QTabWidget.new
        addTab_ tabs "Agenda" =<< newAgendaWidget storage
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

newAgendaWidget :: Storage -> IO QToolBox
newAgendaWidget (Storage dataDir timeVar) = do
    this <- QToolBox.new
    void $ onEvent this $ \(_ :: QShowEvent) ->
        -- TODO set the first item as current
        pure False

    let newSection label = do
            section <- QWidget.new
            _ <- addItem this section label
            box <- QVBoxLayout.newWithParent section
            addStretch box
            pure box

    overdue  <- newSection "Overdue"
    endToday <- newSection "Due today"
    endSoon  <- newSection "Due soon"
    actual   <- newSection "Actual"
    starting <- newSection "Starting soon"
    let modeSections = ModeMap{..}

    let addNote note@NoteView{text} = do
            today <- getUtcToday
            item <- newNoteWidgetWithText $ Text.unpack text
            let section = modeSelect modeSections $ taskMode today note
            n <- noteSectionCount section
            insertWidget section n item

    runStorage dataDir timeVar loadActiveNotes >>= traverse_ addNote

    pure this

newNoteWidgetWithText :: String -> IO QLabel
newNoteWidgetWithText = QLabel.newWithText

-- Because last item is always a stretch.
noteSectionCount :: QLayoutConstPtr layout => layout -> IO Int
noteSectionCount layout = pred <$> count layout
