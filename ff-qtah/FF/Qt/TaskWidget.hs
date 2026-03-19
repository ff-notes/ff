{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Qt.TaskWidget (
    TaskWidget (parent),
    new,
    reload,
) where

import Data.Foldable (for_)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QAbstractButton qualified as QAbstractButton
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QPushButton qualified as QPushButton
import Named (defaults, (!))
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as Storage

import FF (cmdDone, cmdPostpone, fromRgaM, viewNote)
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
import FF.Qt.EDSL (
    Layout (..),
    QBoxLayoutItem (..),
    QFormLayoutItem (..),
    qFrame,
    qHBoxLayout,
    qLabel,
    qScrollArea,
 )

type OnTaskUpdated =
    -- | Keep open task view (e.g. on postpone)
    Bool ->
    EntityView Note ->
    IO ()

data TaskWidget = TaskWidget
    { parent :: QFrame
    , textContent :: QLabel
    , storage :: Storage.Handle
    , start :: DateComponent
    , end :: DateComponent
    , noteId :: IORef (Maybe NoteId)
    , onTaskUpdated :: OnTaskUpdated
    , created :: QLabel
    , updated :: QLabel
    , recurring :: QLabel
    }

new :: Storage.Handle -> OnTaskUpdated -> IO TaskWidget
new storage onTaskUpdated = do
    start <- DateComponent.new
    end <- DateComponent.new

    -- setup UI (TODO xDSL?)
    textContent <-
        qLabel
            ! #alignment Qt.AlignTop
            ! #openExternalLinks True
            ! #textFormat Qt.MarkdownText
            ! #textInteractionFlags Qt.TextBrowserInteraction
            ! #wordWrap True
            ! defaults
    postpone <- QPushButton.newWithText "Postpone"
    done <- QPushButton.newWithText "Done"
    created <- QLabel.new
    updated <- QLabel.new
    recurring <- QLabel.new
    parent <-
        qFrame . QFormLayout $
            [ RowWidget $ qScrollArea textContent
            , StringLayout "Start:" start.parent
            , StringLayout "Deadline:" end.parent
            , StringWidget "Created:" $< created
            , StringWidget "Updated:" $< updated
            , StringWidget "Recurring:" $< recurring
            , RowLayout . qHBoxLayout $
                [Widget $< postpone, Widget $< done, Stretch]
            ]
    -- end setup UI

    noteId <- newIORef Nothing
    let this = TaskWidget{..}
    connect_ postpone QAbstractButton.clickedSignal $ onPostponeClicked this
    connect_ done QAbstractButton.clickedSignal $ onDoneClicked this
    pure this

($<) :: (Applicative f) => (f a -> b) -> a -> b
f $< x = f $ pure x

onPostponeClicked :: TaskWidget -> Bool -> IO ()
onPostponeClicked this _checked = do
    mNoteId <- readIORef this.noteId
    for_ mNoteId \noteId ->
        runStorage this.storage (cmdPostpone noteId) >>= update False this

onDoneClicked :: TaskWidget -> Bool -> IO ()
onDoneClicked this _checked = do
    mNoteId <- readIORef this.noteId
    for_ mNoteId \noteId ->
        runStorage this.storage (cmdDone noteId) >>= update False this

reload :: TaskWidget -> NoteId -> IO ()
reload this noteId = do
    atomicWriteIORef this.noteId $ Just noteId
    runStorage this.storage (loadNote noteId) >>= update True this

update :: Bool -> TaskWidget -> EntityDoc Note -> IO ()
update keepOpen this noteDoc = do
    entity <- runStorage this.storage $ viewNote noteDoc
    let Entity{entityVal = NoteView{note}} = entity
    QLabel.setText this.textContent $ fromRgaM note.note_text
    DateComponent.setDate this.start note.note_start
    DateComponent.setDate this.end note.note_end
    -- TODO created
    -- TODO updated
    QLabel.setText
        this.recurring
        if fromMaybe False note.note_recurring then "Yes" else "No"
    this.onTaskUpdated keepOpen entity
