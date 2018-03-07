{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (LocalTime, getRealLocalTime)
import           Data.Foldable (traverse_)
import           Data.FullMap (FullMap (FullMap))
import qualified Data.FullMap as FullMap
import qualified Data.Text as Text
import           Data.Time (Day, toGregorian)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QAbstractSpinBox (setReadOnly)
import           QApplication (QApplication, new)
import           QBoxLayout (QBoxLayoutPtr, addLayout, addStretch, addWidget,
                             insertWidget)
import           QCloseEvent (QCloseEvent)
import           QCoreApplication (exec, setApplicationName,
                                   setApplicationVersion, setOrganizationDomain,
                                   setOrganizationName)
import           QDate (newWithYearMonthDay)
import           QDateEdit (newWithDate)
import           QDateTimeEdit (setCalendarPopup)
import           QFrame (QFrame, QFrameShape (StyledPanel), new, setFrameShape)
import           QHBoxLayout (QHBoxLayout, new)
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
import           FF.Types (ModeMap, NoteView (..), TaskMode (..), taskMode)

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
sectionLabels = FullMap $ \case
    Overdue  -> "Overdue"
    EndToday -> "Due today"
    EndSoon  -> "Due soon"
    Actual   -> "Actual"
    Starting -> "Starting soon"

sectionIndices :: ModeMap Int
sectionIndices = FullMap fromEnum

sectionLabel :: TaskMode -> Int -> String
sectionLabel mode n =
    concat [FullMap.lookup mode sectionLabels, " (", show n, ")"]

newAgendaWidget :: Storage -> IO QToolBox
newAgendaWidget (Storage dataDir timeVar) = do
    this <- QToolBox.new
    void $ onEvent this $ \(_ :: QShowEvent) ->
        -- TODO set the first item as current
                                                pure False
    modeSections <- sequence . FullMap $ newSection this
    runStorage dataDir timeVar loadActiveNotes
        >>= traverse_ (addNote this modeSections)
    pure this

newSection :: QToolBoxPtr toolbox => toolbox -> TaskMode -> IO QVBoxLayout
newSection this section = do
    widget <- QWidget.new
    _      <- addItem this widget label
    box    <- QVBoxLayout.newWithParent widget
    addStretch box
    pure box
    where label = sectionLabel section 0

addNote
    :: QBoxLayoutPtr layout => QToolBox -> ModeMap layout -> NoteView -> IO ()
addNote this modeSections note = do
    today <- getUtcToday
    item  <- newNoteWidget note
    let mode       = taskMode today note
    let sectionBox = FullMap.lookup mode modeSections
    n <- sectionSize sectionBox
    insertWidget sectionBox n item
    setItemText this
                (FullMap.lookup mode sectionIndices)
                (sectionLabel mode $ n + 1)

newDateWidget :: String -> Day -> IO QHBoxLayout
newDateWidget label date = do
    box <- QHBoxLayout.new
    addWidget box =<< QLabel.newWithText label
    addWidget box =<< do
        dateEdit <-
            QDateEdit.newWithDate
                =<< QDate.newWithYearMonthDay (fromInteger y) m d
        setCalendarPopup dateEdit True
        setReadOnly      dateEdit True
        pure dateEdit
    pure box
    where (y, m, d) = toGregorian date

newNoteWidget :: NoteView -> IO QFrame
newNoteWidget NoteView { text, start, end } = do
    this <- QFrame.new
    setFrameShape this StyledPanel
    do
        box <- QVBoxLayout.newWithParent this
        addWidget box =<< QLabel.newWithText (Text.unpack text)
        addLayout box =<< do
            fieldsBox <- QHBoxLayout.new
            addLayout fieldsBox =<< newDateWidget "Start:" start
            case end of
                Just endDate ->
                    addLayout fieldsBox =<< newDateWidget "Deadline:" endDate
                Nothing -> pure ()
            addStretch fieldsBox
            pure fieldsBox
    pure this

-- Because last item is always a stretch.
sectionSize :: QLayoutConstPtr layout => layout -> IO Int
sectionSize layout = pred <$> count layout
