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

import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Function (fix)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime)
import Foreign.Hoppy.Runtime (CppPtr, delete, nullptr)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QAbstractButton qualified as QAbstractButton
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QLayout (QLayoutPtr)
import Graphics.UI.Qtah.Widgets.QLayout qualified as QLayout
import Graphics.UI.Qtah.Widgets.QLayoutItem qualified as QLayoutItem
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
    View (NoteView),
    loadNote,
 )
import FF.Types qualified

import FF.Qt (hDateFormat)
import FF.Qt.DateComponent (DateComponent)
import FF.Qt.DateComponent qualified as DateComponent
import FF.Qt.EDSL (
    Layout (..),
    QBoxLayoutItem (..),
    QFormLayoutItem (..),
    qFrame,
    qHBoxLayout,
    qLabel,
    qPushButton,
    qScrollArea,
    ($<),
 )

type OnTaskUpdated =
    -- | Keep open task view (e.g. on postpone)
    Bool ->
    EntityView Note ->
    IO ()

data TaskWidget = TaskWidget
    { storage :: Storage.Handle
    , noteId :: IORef (Maybe NoteId)
    , onTaskUpdated :: OnTaskUpdated
    , -- UI
      parent :: QFrame
    , textContent :: QLabel
    , tags :: QHBoxLayout
    , start :: DateComponent
    , end :: DateComponent
    , created :: QLabel
    , updated :: QLabel
    , recurring :: QLabel
    }

new :: Storage.Handle -> OnTaskUpdated -> IO TaskWidget
new storage onTaskUpdated = do
    start <- DateComponent.new
    end <- DateComponent.new

    textContent <-
        qLabel
            ! #alignment Qt.AlignTop
            ! #objectName "textContent"
            ! #openExternalLinks True
            ! #textFormat Qt.MarkdownText
            ! #textInteractionFlags Qt.textBrowserInteraction
            ! #wordWrap True
            ! defaults
    tags <- qHBoxLayout ! #spacing 0 ! defaults $ []
    postpone <-
        qPushButton ! #objectName "postpone" ! #text "Postpone" ! defaults
    done <- qPushButton ! #objectName "done" ! #text "Done" ! defaults
    created <- qLabel ! #objectName "created" ! defaults
    updated <- qLabel ! #objectName "updated" ! defaults
    recurring <- qLabel ! #objectName "recurring" ! defaults
    parent <-
        qFrame ! #objectName "parent" $
            QFormLayout
                [ RowWidget $ qScrollArea textContent
                , StringLayout "Tags:" $< tags
                , StringLayout "Start:" $< start.parent
                , StringLayout "Deadline:" $< end.parent
                , StringWidget "Created:" $< created
                , StringWidget "Updated:" $< updated
                , StringWidget "Recurring:" $< recurring
                , RowLayout $
                    qHBoxLayout ! #objectName "actionsRow" ! defaults $
                        [Widget $< postpone, Widget $< done, Stretch]
                ]

    noteId <- newIORef Nothing
    let this = TaskWidget{..}
    connect_ postpone QAbstractButton.clickedSignal $ onPostponeClicked this
    connect_ done QAbstractButton.clickedSignal $ onDoneClicked this
    pure this

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
    let Entity{entityVal = view@NoteView{note}} = entity
    QLabel.setText this.textContent $ fromRgaM note.note_text
    resetTags view.tags
    DateComponent.setDate this.start note.note_start
    DateComponent.setDate this.end note.note_end
    QLabel.setText this.created $
        formatTime defaultTimeLocale hDateFormat view.created
    QLabel.setText this.updated $
        formatTime defaultTimeLocale hDateFormat view.lastUpdated
    QLabel.setText
        this.recurring
        if fromMaybe False note.note_recurring then "Yes" else "No"
    this.onTaskUpdated keepOpen entity
  where
    resetTags tags = do
        deleteAllLayoutWidgets this.tags
        for_ tags \tag ->
            void $
                QBoxLayout.addWidget this.tags
                    =<< qPushButton ! #text (Text.unpack tag) ! defaults

deleteAllLayoutWidgets :: (QLayoutPtr layout) => layout -> IO ()
deleteAllLayoutWidgets layout =
    whilePtrAlive (QLayout.takeAt layout 0) \childItem -> do
        delete =<< QLayoutItem.widget childItem
        delete childItem

whilePtrAlive :: (CppPtr t, Eq t, Monad m) => m t -> (t -> m a) -> m ()
whilePtrAlive get action =
    fix \w -> do x <- get; when (x /= nullptr) $ action x *> w
