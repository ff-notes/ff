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

import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QAbstractButton qualified as QAbstractButton
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QPushButton qualified as QPushButton
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QSizePolicy (
    QSizePolicyPolicy (..),
 )
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import Named ((!))
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
import FF.Qt.EDSL (
    QBoxLayoutItem (..),
    QFormLayoutItem (..),
    hline,
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
    { parent :: QScrollArea
    , innerWidget :: QFrame
    -- ^ the main widget inside the scroll area
    , textContent :: QLabel
    , storage :: Storage.Handle
    , start :: DateComponent
    , end :: DateComponent
    , noteId :: IORef (Maybe NoteId)
    , onTaskUpdated :: OnTaskUpdated
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
            ! #sizePolicy (MinimumExpanding, MinimumExpanding)
            ! #textInteractionFlags Qt.TextBrowserInteraction
            ! #textFormat Qt.MarkdownText
            ! #wordWrap True
    postpone <- QPushButton.newWithText "Postpone"
    innerWidget <-
        qFrame
            [ RowWidget $ pure textContent
            , RowWidget hline
            , StringLayout "Start:" start.parent
            , StringLayout "Deadline:" end.parent
            , RowLayout $ qHBoxLayout [Widget $ pure postpone, Stretch]
            ]
    parent <- qScrollArea innerWidget
    -- end setup UI

    noteId <- newIORef Nothing
    let this = TaskWidget{..}
    connect_ postpone QAbstractButton.clickedSignal $ postponeSlot this
    pure this

postponeSlot :: TaskWidget -> Bool -> IO ()
postponeSlot this _checked = do
    mNoteId <- readIORef this.noteId
    case mNoteId of
        Just noteId ->
            runStorage this.storage (cmdPostpone noteId) >>= update False this
        Nothing -> pure ()

reload :: TaskWidget -> NoteId -> IO ()
reload this noteId = do
    atomicWriteIORef this.noteId $ Just noteId
    runStorage this.storage (loadNote noteId) >>= update True this

update :: Bool -> TaskWidget -> EntityDoc Note -> IO ()
update keepOpen this noteDoc = do
    entity <- runStorage this.storage $ viewNote noteDoc
    let Entity{entityVal} = entity
    let NoteView{note} = entityVal
    let Note{note_text, note_start, note_end} = note
    QLabel.setText this.textContent $ fromRgaM note_text
    DateComponent.setDate this.start note_start
    DateComponent.setDate this.end note_end
    QWidget.adjustSize this.innerWidget
    this.onTaskUpdated keepOpen entity
