{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Extra (void, whenJust, (<=<))
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.Time (Day, toGregorian)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           QAbstractButton (setText)
import           QAbstractSpinBox (setReadOnly)
import           QAction (triggeredSignal)
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
import           QMainWindow (QMainWindow, QMainWindowPtr, new, restoreState,
                              saveState, setCentralWidget)
import           QMenu (QMenu, addNewAction)
import qualified QMenu
import           QSettings (new, setValue, value)
import           QShowEvent (QShowEvent)
import           QString (QStringValue)
import           QTabWidget (QTabWidget, addTab, new)
import           Qtah.Event (onEvent)
import           Qtah.Signal (connect_)
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
import           RON.Storage.IO (docIdFromUuid, runStorage)
import qualified RON.Storage.IO as Storage
import           System.Environment (getArgs)

import           FF (getDataDir, getUtcToday, loadActiveTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId, TaskMode (Actual, EndSoon, EndToday, Overdue, Starting),
                           entityId, entityVal, note_end, note_start, note_text,
                           taskMode)

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
    installWindowStateSaver this
    pure this

-- | https://wiki.qt.io/Saving_Window_Size_State
installWindowStateSaver :: QMainWindowPtr window => window -> IO ()
installWindowStateSaver this = do
    onEvent_ $ \(_ :: QShowEvent) ->
        withScopedPtr QSettings.new $ \settings -> do
            value' settings "mainWindowGeometry" >>= restoreGeometry_
            value' settings "mainWindowState"    >>= restoreState_
    onEvent_ $ \(_ :: QCloseEvent) ->
        withScopedPtr QSettings.new $ \settings -> do
            setValue' settings "mainWindowGeometry" =<< saveGeometry this
            setValue' settings "mainWindowState"    =<< saveState this
  where
    onEvent_ handler = void $ onEvent this $ ($> False) . handler
    restoreGeometry_ = void . restoreGeometry this
    restoreState_    = void . restoreState    this
    value'    settings key = value    settings key >>= toByteArray
    setValue' settings key = setValue settings key <=< QVariant.newWithByteArray

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
    runStorage h loadActiveTasks >>= traverse_ (addTask this modeSections)
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

addTask :: QToolBox -> (TaskMode -> QVBoxLayout) -> Entity Note -> IO ()
addTask this modeSections eTask@Entity{entityVal=task} = do
    today    <- getUtcToday
    taskItem <- newTaskWidget eTask
    let mode    = taskMode today task
    let section = modeSections mode
    taskCount <- sectionSize section
    insertWidget section taskCount taskItem
    sectionWidget <- QLayout.parentWidget section
    sectionIndex  <- indexOf this sectionWidget
    setItemText this sectionIndex (sectionLabel mode $ taskCount + 1)

newDateWidget :: String -> Day -> IO QHBoxLayout
newDateWidget label date = do
    box <- QHBoxLayout.new
    addWidget box =<< QLabel.newWithText label
    addWidget box =<< do
        dateEdit <-
            QDateEdit.newWithDate =<< QDate.newWithYmd (fromInteger y) m d
        setCalendarPopup dateEdit True
        setReadOnly      dateEdit True
        pure dateEdit
    pure box
  where
    (y, m, d) = toGregorian date

newTaskWidget :: Entity Note -> IO QFrame
newTaskWidget Entity{entityId, entityVal} = do
    this <- QFrame.new
    setFrameShape this StyledPanel
    do  -- box
        box <- QVBoxLayout.newWithParent this
        addWidget box =<< QLabel.newWithText note_text
        addLayout box =<< do
            fieldsBox <- QHBoxLayout.new
            addLayout fieldsBox =<< newDateWidget "Start:" note_start
            whenJust note_end $
                addLayout fieldsBox <=< newDateWidget "Deadline:"
            addStretch fieldsBox
            addWidget fieldsBox =<< newTaskActionsButton taskId
            pure fieldsBox
    pure this
  where
    Note{note_text, note_start, note_end} = entityVal
    taskId = docIdFromUuid entityId

-- Because last item is always a stretch.
sectionSize :: QLayoutConstPtr layout => layout -> IO Int
sectionSize layout = pred <$> count layout

newTaskActionsButton :: NoteId -> IO QToolButton
newTaskActionsButton taskId = do
    this <- QToolButton.new
    setText this "⋮"
    setPopupMode this InstantPopup
    setMenu this =<< do
        menu <- QMenu.new
        addAction menu "Postpone" $ print taskId
        pure menu
    pure this

addAction :: QStringValue string => QMenu -> string -> IO () -> IO ()
addAction menu text handler = do
    action <- addNewAction menu text
    connect_ action triggeredSignal $ const handler
