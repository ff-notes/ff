{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (LocalTime, getRealLocalTime)
import           Data.Foldable (traverse_)
import qualified Data.Text as Text
import           Data.Time (toGregorian)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QApplication (QApplication, new)
import           QBoxLayout (QBoxLayoutPtr, addStretch, addWidget, insertWidget)
import           QCloseEvent (QCloseEvent)
import           QCoreApplication (exec, setApplicationName,
                                   setApplicationVersion, setOrganizationDomain,
                                   setOrganizationName)
import           QDate (newWithYearMonthDay)
import           QDateEdit (newWithDate)
import           QFrame (QFrame, QFrameShape (StyledPanel), new, setFrameShape)
import           QLabel (newWithText)
import           QLayout (QLayoutConstPtr, count)
import           QMainWindow (QMainWindow, new, restoreState, saveState,
                              setCentralWidget)
import           QSettings (new, setValue, value)
import           QShowEvent (QShowEvent)
import           QTabWidget (QTabWidget, addTab, new)
import           Qtah.Event (onEvent)
import           QToolBox (QToolBox, QToolBoxPtr, addItem, new, setItemText)
import           QVariant (newWithByteArray, toByteArray)
import           QVBoxLayout (QVBoxLayout, newWithParent)
import           QWidget (QWidgetPtr, new, restoreGeometry, saveGeometry,
                          setWindowTitle)
import qualified QWidget
import           System.Environment (getArgs)

import           FF (getUtcToday, loadActiveNotes)
import           FF.Config (Config (Config, dataDir), loadConfig)
import           FF.Storage (runStorage)
import           FF.Types (ModeMap (..), NoteView (..), TaskMode (..),
                           modeSelect, taskMode)

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

sectionLabels :: ModeMap String
sectionLabels = ModeMap
    { overdue  = "Overdue"
    , endToday = "Due today"
    , endSoon  = "Due soon"
    , actual   = "Actual"
    , starting = "Starting soon"
    }

sectionIndices :: ModeMap Int
sectionIndices = ModeMap
    { overdue  = 0
    , endToday = 1
    , endSoon  = 2
    , actual   = 3
    , starting = 4
    }

sectionLabel :: TaskMode -> Int -> String
sectionLabel mode n = concat [modeSelect sectionLabels mode, " (", show n, ")"]

newAgendaWidget :: Storage -> IO QToolBox
newAgendaWidget (Storage dataDir timeVar) = do
    this <- QToolBox.new
    void $ onEvent this $ \(_ :: QShowEvent) ->
        -- TODO set the first item as current
        pure False

    overdue  <- newSection this Overdue
    endToday <- newSection this EndToday
    endSoon  <- newSection this EndSoon
    actual   <- newSection this Actual
    starting <- newSection this Starting
    let modeSections = ModeMap{..}

    runStorage dataDir timeVar loadActiveNotes
        >>= traverse_ (addNote this modeSections)

    pure this

newSection :: QToolBoxPtr toolbox => toolbox -> TaskMode -> IO QVBoxLayout
newSection this section = do
    widget <- QWidget.new
    _ <- addItem this widget label
    box <- QVBoxLayout.newWithParent widget
    addStretch box
    pure box
  where
    label = sectionLabel section 0

addNote
    :: (QToolBoxPtr toolbox, QBoxLayoutPtr layout)
    => toolbox -> ModeMap layout -> NoteView -> IO ()
addNote this modeSections note = do
    today <- getUtcToday
    item <- newNoteWidget note
    let mode = taskMode today note
    let sectionBox = modeSelect modeSections mode
    n <- sectionSize sectionBox
    insertWidget sectionBox n item
    setItemText
        this (modeSelect sectionIndices mode) (sectionLabel mode $ n + 1)

newNoteWidget :: NoteView -> IO QFrame
newNoteWidget NoteView{text, start} = do
    this <- QFrame.new
    setFrameShape this StyledPanel
    do  box <- QVBoxLayout.newWithParent this
        addWidget box =<< QLabel.newWithText (Text.unpack text)
        addWidget box
            =<< QDateEdit.newWithDate
            =<< QDate.newWithYearMonthDay
                    (fromInteger startYear) startMonth startDay
    pure this
  where
    (startYear, startMonth, startDay) = toGregorian start

-- Because last item is always a stretch.
sectionSize :: QLayoutConstPtr layout => layout -> IO Int
sectionSize layout = pred <$> count layout
