{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module FF.Qt.TaskWidget (
    TaskWidget (super),
    new,
    update,
) where

-- global
import Foreign.Hoppy.Runtime (toGc)
import Graphics.UI.Qtah.Core.Types (QtAlignmentFlag (AlignTop))
import Graphics.UI.Qtah.Widgets.QFormLayout qualified as QFormLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicy, QSizePolicyPolicy)
import Graphics.UI.Qtah.Widgets.QSizePolicy qualified as QSizePolicy
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as Storage

-- project
import FF (fromRgaM, viewNote)
import FF.Types (
    Entity (..),
    Note (..),
    NoteId,
    View (NoteView, note),
    loadNote,
 )

-- package
import FF.Qt.DateComponent (DateComponent)
import FF.Qt.DateComponent qualified as DateComponent

data TaskWidget = TaskWidget
    { super :: QScrollArea
    , frame :: QFrame
    -- ^ widget inside the scroll area
    , textContent :: QLabel
    -- ^ label for the text
    , storage :: Storage.Handle
    , start :: DateComponent
    , end :: DateComponent
    }

new :: Storage.Handle -> IO TaskWidget
new storage = do
    super <- QScrollArea.new

    frame <- QFrame.new
    QScrollArea.setWidget super frame

    textContent <- QLabel.new
    QWidget.setSizePolicy textContent
        =<< makeSimpleSizePolicy QSizePolicy.MinimumExpanding
    QLabel.setAlignment textContent AlignTop
    QLabel.setWordWrap textContent True

    start <- DateComponent.new
    end <- DateComponent.new

    form <- QFormLayout.newWithParent frame
    QFormLayout.addRowWidget form textContent
    QFormLayout.addRowStringWidget form "Start:" start.dateEdit
    QFormLayout.addRowStringWidget form "Deadline:" end.dateEdit

    pure TaskWidget{super, frame, textContent, storage, start, end}

update :: TaskWidget -> NoteId -> IO ()
update this noteId = do
    Entity{entityVal} <- runStorage this.storage $ loadNote noteId >>= viewNote
    let NoteView{note} = entityVal
    let Note{note_text, note_start, note_end} = note
    QLabel.setText this.textContent $ fromRgaM note_text
    DateComponent.setDate this.start note_start
    DateComponent.setDate this.end note_end
    QWidget.adjustSize this.frame

makeSimpleSizePolicy :: QSizePolicyPolicy -> IO QSizePolicy
makeSimpleSizePolicy policy =
    toGc =<< QSizePolicy.newWithOptions policy policy QSizePolicy.DefaultType
