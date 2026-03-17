{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module FF.Qt.TaskWidget (
    TaskWidget (parent),
    new,
    reload,
) where

import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Foreign.Hoppy.Runtime (toGc)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QAbstractButton qualified as QAbstractButton
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFormLayout qualified as QFormLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QHBoxLayout qualified as QHBoxLayout
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QPushButton qualified as QPushButton
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicy, QSizePolicyPolicy)
import Graphics.UI.Qtah.Widgets.QSizePolicy qualified as QSizePolicy
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as Storage

import FF (cmdPostpone, fromRgaM, viewNote)
import FF.Types (
    Entity (..),
    EntityDoc,
    EntityView,
    Note (..),
    NoteId,
    View (NoteView, note),
    loadNote,
 )

import FF.Qt.DateComponent (DateComponent)
import FF.Qt.DateComponent qualified as DateComponent

data TaskWidget = TaskWidget
    { parent :: QScrollArea
    , innerWidget :: QFrame
    -- ^ the main widget inside the scroll area
    , textContent :: QLabel
    , storage :: Storage.Handle
    , start :: DateComponent
    , end :: DateComponent
    , noteId :: IORef (Maybe NoteId)
    , onTaskUpdated :: EntityView Note -> IO ()
    }

new :: Storage.Handle -> (EntityView Note -> IO ()) -> IO TaskWidget
new storage onTaskUpdated = do
    parent <- QScrollArea.new

    innerWidget <- QFrame.new
    QScrollArea.setWidget parent innerWidget

    textContent <- QLabel.new
    QWidget.setSizePolicy textContent
        =<< makeSimpleSizePolicy QSizePolicy.MinimumExpanding
    QLabel.setAlignment textContent Qt.AlignTop
    QLabel.setWordWrap textContent True

    hline <- QFrame.new
    QFrame.setFrameShape hline QFrame.HLine

    start <- DateComponent.new

    end <- DateComponent.new

    form <- QFormLayout.newWithParent innerWidget
    QFormLayout.addRowWidget form textContent
    QFormLayout.addRowWidget form hline
    QFormLayout.addRowStringLayout form "Start:" start.parent
    QFormLayout.addRowStringLayout form "Deadline:" end.parent

    postpone <- QPushButton.newWithText "Postpone"

    actions <- QHBoxLayout.new
    QBoxLayout.addWidget actions postpone
    QBoxLayout.addStretch actions
    QFormLayout.addRowLayout form actions

    noteId <- newIORef Nothing

    let this =
            TaskWidget
                { parent
                , innerWidget
                , textContent
                , storage
                , start
                , end
                , noteId
                , onTaskUpdated
                }

    connect_ postpone QAbstractButton.clickedSignal $ postponeSlot this

    pure this

postponeSlot :: TaskWidget -> Bool -> IO ()
postponeSlot this _checked = do
    mNoteId <- readIORef this.noteId
    case mNoteId of
        Just noteId ->
            runStorage this.storage (cmdPostpone noteId) >>= update this
        Nothing -> pure ()

reload :: TaskWidget -> NoteId -> IO ()
reload this noteId = do
    atomicWriteIORef this.noteId $ Just noteId
    runStorage this.storage (loadNote noteId) >>= update this

update :: TaskWidget -> EntityDoc Note -> IO ()
update this noteDoc = do
    entity <- runStorage this.storage $ viewNote noteDoc
    let Entity{entityVal} = entity
    let NoteView{note} = entityVal
    let Note{note_text, note_start, note_end} = note
    QLabel.setText this.textContent $ fromRgaM note_text
    DateComponent.setDate this.start note_start
    DateComponent.setDate this.end note_end
    QWidget.adjustSize this.innerWidget
    this.onTaskUpdated entity

makeSimpleSizePolicy :: QSizePolicyPolicy -> IO QSizePolicy
makeSimpleSizePolicy policy =
    toGc =<< QSizePolicy.newWithOptions policy policy QSizePolicy.DefaultType
