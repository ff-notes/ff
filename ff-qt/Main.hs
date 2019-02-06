{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Extra (void, whenJust, (<=<))
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time (Day, toGregorian)
import           Data.Typeable (cast, typeRep)
import           Data.Version (showVersion)
import           Foreign.Hoppy.Runtime (delete, withScopedPtr)
import           GHC.Stack (HasCallStack)
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
import           QLayout (QLayoutConstPtr, count, parentWidget)
import           QMainWindow (QMainWindow, QMainWindowPtr, new, restoreState,
                              saveState, setCentralWidget)
import           QMenu (QMenu, addNewAction, new)
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
                              new, setMenu, setPopupMode)
import           QVariant (newWithByteArray, toByteArray)
import           QVBoxLayout (QVBoxLayout, newWithParent)
import           QWidget (QWidgetPtr, new, restoreGeometry, saveGeometry,
                          setWindowTitle)
import qualified QWidget
import           RON.Storage.IO (Collection, CollectionDocId (CollectionDocId),
                                 DocId, runStorage, subscribeForever)
import qualified RON.Storage.IO as Storage
import           System.Environment (getArgs)

import           FF (cmdPostpone, getDataDir, getUtcToday, load,
                     loadActiveTasks)
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

        app@App{mainWindow, storage} <- newMainWindow h
        void $ forkIO $
            subscribeForever storage $
                \(CollectionDocId docId) -> updateView app docId
        QWidget.show mainWindow
        exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

type AgendaSection = QVBoxLayout

data App = App
    { agendaModeSections :: ! (TaskMode -> AgendaSection)
    , agendaTaskWidgets  :: ! (IORef (Map NoteId QFrame))
    , agendaWidget       :: ! QToolBox
    , mainWindow         :: ! QMainWindow
    , storage            :: ! Storage.Handle
    }

newMainWindow :: Storage.Handle -> IO App
newMainWindow h = do
    this <- QMainWindow.new
    app@App{agendaWidget} <- newAgendaWidget this h
    setCentralWidget this =<< do
        tabs <- QTabWidget.new
        addTab_ tabs "Agenda" agendaWidget
        pure tabs
    setWindowTitle this "ff"
    installWindowStateSaver this
    pure app

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

newAgendaWidget :: QMainWindow -> Storage.Handle -> IO App
newAgendaWidget qMainWindow h = do
    this <- QToolBox.new
    void $ onEvent this $ \(_ :: QShowEvent) ->
        -- TODO set the first item as current
        pure False
    agendaModeSections <- do
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
    agendaTaskWidgets <- newIORef mempty
    let app = App
            { agendaModeSections
            , agendaTaskWidgets
            , agendaWidget = this
            , mainWindow = qMainWindow
            , storage = h
            }
    runStorage h loadActiveTasks >>= traverse_ (addTask app)
    pure app

newSection :: QToolBoxPtr toolbox => toolbox -> TaskMode -> IO AgendaSection
newSection this section = do
    widget <- QWidget.new
    _      <- addItem this widget label
    box    <- QVBoxLayout.newWithParent widget
    addStretch box
    pure box
  where
    label = sectionLabel section 0

addTask :: App -> Entity Note -> IO ()
addTask mainWindow taskEntity = do
    today      <- getUtcToday
    taskWidget <- newTaskWidget h taskEntity
    let mode    = taskMode today task
    let section = agendaModeSections mode
    taskCount <- sectionSize section
    insertWidget section taskCount taskWidget
    sectionWidget <- QLayout.parentWidget section
    sectionIndex  <- indexOf agendaWidget sectionWidget
    setItemText agendaWidget sectionIndex (sectionLabel mode $ taskCount + 1)

    mWidget <- Map.lookup taskId <$> readIORef agendaTaskWidgets
    whenJust mWidget delete  -- TODO update old section counter
    modifyIORef agendaTaskWidgets $ Map.insert taskId taskWidget

  where
    Entity{entityId=taskId, entityVal=task} = taskEntity
    App
            { agendaWidget
            , agendaModeSections
            , agendaTaskWidgets
            , storage = h
            } =
        mainWindow

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

newTaskWidget :: Storage.Handle -> Entity Note -> IO QFrame
newTaskWidget h Entity{entityId, entityVal} = do
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
            addWidget fieldsBox =<< newTaskActionsButton h entityId
            addStretch fieldsBox
            pure fieldsBox
    pure this
  where
    Note{note_text, note_start, note_end} = entityVal

-- Because last item is always a stretch.
sectionSize :: QLayoutConstPtr layout => layout -> IO Int
sectionSize layout = pred <$> count layout

newTaskActionsButton :: Storage.Handle -> NoteId -> IO QToolButton
newTaskActionsButton h taskId = do
    this <- QToolButton.new
    setText this "⋮"
    setPopupMode this InstantPopup
    setMenu this =<< do
        menu <- QMenu.new
        addAction menu "Postpone" $ runStorage h $ cmdPostpone taskId
        pure menu
    pure this

addAction :: QStringValue string => QMenu -> string -> IO a -> IO ()
addAction menu text handler = do
    action <- addNewAction menu text
    connect_ action triggeredSignal $ const $ void handler

updateView :: (HasCallStack, Collection a) => App -> DocId a -> IO ()
updateView mainWindow docid = case docid of
    (cast -> Just noteId) -> updateTask mainWindow noteId
    _ -> error $ show (typeRep docid, docid)

updateTask :: App -> NoteId -> IO ()
updateTask mainWindow noteId = do
    note <- runStorage h $ load noteId
    addTask   mainWindow note
  where
    App{storage = h} = mainWindow
