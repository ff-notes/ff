{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Extra (void, whenJust, (<=<))
import           Data.Foldable (traverse_)
import           Data.Time (Day, toGregorian)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QAbstractButton (setText)
import           QAbstractSpinBox (setReadOnly)
import           QApplication (QApplication, new)
import           QBoxLayout (addLayout, addStretch, addWidget, insertWidget)
import           QCloseEvent (QCloseEvent)
import           QCoreApplication (exec, setApplicationName,
                                   setApplicationVersion, setOrganizationDomain,
                                   setOrganizationName)
import           QDate (newWithYmd)
import           QDateEdit (newWithDate)
import           QDateTimeEdit (setCalendarPopup)
import           QFrame (QFrame, QFrameShape (StyledPanel), new, setFrameShape)
import           QHBoxLayout (QHBoxLayout, new)
import           QLabel (newWithText)
import           QLayout (QLayoutConstPtr, count)
import qualified QLayout
import           QMainWindow (QMainWindow, new, restoreState, saveState,
                              setCentralWidget)
import           QMenu (addNewAction)
import qualified QMenu
import           QSettings (new, setValue, value)
import           QShowEvent (QShowEvent)
import           QTabWidget (QTabWidget, addTab, new)
import           Qtah.Event (onEvent)
import           QToolBox (QToolBox, QToolBoxPtr, addItem, indexOf, new,
                           setItemText)
import           QToolButton (QToolButton,
                              QToolButtonToolButtonPopupMode (InstantPopup),
                              setMenu, setPopupMode)
import qualified QToolButton
import           QVariant (newWithByteArray, toByteArray)
import           QVBoxLayout (QVBoxLayout, newWithParent)
import           QWidget (QWidgetPtr, new, restoreGeometry, saveGeometry,
                          setWindowTitle)
import qualified QWidget
import           RON.Storage.IO (runStorage)
import qualified RON.Storage.IO as Storage
import           System.Environment (getArgs)

import           FF (getDataDir, getUtcToday, loadActiveNotes)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), TaskMode (Actual, EndSoon, EndToday, Overdue, Starting),
                           entityVal, note_end, note_start, note_text, taskMode)

import           Paths_ff_qt (version)

main :: IO ()
main = do
    cfg     <- loadConfig
    dataDir <- getDataDir cfg
    h       <- Storage.newHandle dataDir
    withApp $ \_ -> do
        setOrganizationDomain   "ff.cblp.su"
        setOrganizationName     "ff"
        setApplicationName      "ff"
        setApplicationVersion $ showVersion version

        mainWindow <- newMainWindow h
        QWidget.show mainWindow
        exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

newMainWindow :: Storage.Handle -> IO QMainWindow
newMainWindow h = do
    this <- QMainWindow.new
    setCentralWidget this =<< do
        tabs <- QTabWidget.new
        addTab_ tabs "Agenda" =<< newAgendaWidget h
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

sectionLabel :: TaskMode -> Int -> String
sectionLabel mode n = let
    label = case mode of
        Overdue  _ -> "Overdue"
        EndToday   -> "Due today"
        EndSoon  _ -> "Due soon"
        Actual     -> "Actual"
        Starting _ -> "Starting soon"
    in concat [label, " (", show n, ")"]

newAgendaWidget :: Storage.Handle -> IO QToolBox
newAgendaWidget h = do
    this <- QToolBox.new
    void $ onEvent this $ \(_ :: QShowEvent) ->
        -- TODO set the first item as current
        pure False
    modeSections <- do
        od <- newSection this $ Overdue  undefined
        et <- newSection this   EndToday
        es <- newSection this $ EndSoon  undefined
        ac <- newSection this   Actual
        st <- newSection this $ Starting undefined
        pure $ \case
            Overdue  _ -> od
            EndToday   -> et
            EndSoon  _ -> es
            Actual     -> ac
            Starting _ -> st
    runStorage h loadActiveNotes >>= traverse_ (addNote this modeSections)
    pure this

newSection :: QToolBoxPtr toolbox => toolbox -> TaskMode -> IO QVBoxLayout
newSection this section = do
    widget <- QWidget.new
    _      <- addItem this widget label
    box    <- QVBoxLayout.newWithParent widget
    addStretch box
    pure box
  where
    label = sectionLabel section 0

addNote :: QToolBox -> (TaskMode -> QVBoxLayout) -> Entity Note -> IO ()
addNote this modeSections eNote@Entity{entityVal=note} = do
    today    <- getUtcToday
    noteItem <- newNoteWidget eNote
    let mode    = taskMode today note
    let section = modeSections mode
    noteCount <- sectionSize section
    insertWidget section noteCount noteItem
    sectionWidget <- QLayout.parentWidget section
    sectionIndex <- indexOf this sectionWidget
    setItemText this sectionIndex (sectionLabel mode $ noteCount + 1)

newDateWidget :: String -> Day -> IO QHBoxLayout
newDateWidget label date = do
    box <- QHBoxLayout.new
    addWidget box =<< QLabel.newWithText label
    addWidget box =<< do
        dateEdit <-
            QDateEdit.newWithDate
                =<< QDate.newWithYmd (fromInteger y) m d
        setCalendarPopup dateEdit True
        setReadOnly      dateEdit True
        pure dateEdit
    pure box
  where
    (y, m, d) = toGregorian date

newNoteWidget :: Entity Note -> IO QFrame
newNoteWidget Entity{entityVal = Note{note_text, note_start, note_end}} = do
    this <- QFrame.new
    setFrameShape this StyledPanel
    do
        box <- QVBoxLayout.newWithParent this
        addWidget box =<< QLabel.newWithText note_text
        addLayout box =<< do
            fieldsBox <- QHBoxLayout.new
            addLayout fieldsBox =<< newDateWidget "Start:" note_start
            whenJust note_end $
                addLayout fieldsBox <=< newDateWidget "Deadline:"
            addStretch fieldsBox
            addWidget fieldsBox =<< newTaskActionsButton
            pure fieldsBox
    pure this

-- Because last item is always a stretch.
sectionSize :: QLayoutConstPtr layout => layout -> IO Int
sectionSize layout = pred <$> count layout

newTaskActionsButton :: IO QToolButton
newTaskActionsButton = do
    this <- QToolButton.new
    setText this "⋮"
    setPopupMode this InstantPopup
    setMenu this =<< do
        menu <- QMenu.new
        _ <- addNewAction menu "Postpone"
        pure menu
    pure this
