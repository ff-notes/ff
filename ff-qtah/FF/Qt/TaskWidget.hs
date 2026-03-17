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
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicy, QSizePolicyPolicy)
import Graphics.UI.Qtah.Widgets.QSizePolicy qualified as QSizePolicy
import Graphics.UI.Qtah.Widgets.QVBoxLayout qualified as QVBoxLayout
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as Storage

-- project
import FF (fromRgaM, viewNote)
import FF.Types (
    Entity (..),
    Note (Note, note_text),
    NoteId,
    View (NoteView, note),
    loadNote,
 )

-- package
import FF.Qt.DateComponent qualified as DateComponent

data TaskWidget = TaskWidget
    { super :: QScrollArea
    , frame :: QFrame
    , label :: QLabel
    , storage :: Storage.Handle
    }

new :: Storage.Handle -> IO TaskWidget
new storage = do
    super <- QScrollArea.new

    frame <- QFrame.new
    QScrollArea.setWidget super frame

    label <- QLabel.new
    QWidget.setSizePolicy label
        =<< makeSimpleSizePolicy QSizePolicy.MinimumExpanding
    QLabel.setAlignment label AlignTop
    QLabel.setWordWrap label True

    start <- DateComponent.new "Start:"
    end <- DateComponent.new "Deadline:"

    box <- QVBoxLayout.newWithParent frame
    QBoxLayout.addWidget box label
    QBoxLayout.addLayout box start.super
    QBoxLayout.addLayout box end.super

    pure TaskWidget{super, frame, label, storage}

update :: TaskWidget -> NoteId -> IO ()
update TaskWidget{frame, label, storage} noteId = do
    Entity{entityVal} <- runStorage storage $ loadNote noteId >>= viewNote
    let NoteView{note} = entityVal
    let Note{note_text} = note
    QLabel.setText label $ fromRgaM note_text
    QWidget.adjustSize frame

makeSimpleSizePolicy :: QSizePolicyPolicy -> IO QSizePolicy
makeSimpleSizePolicy policy =
    toGc =<< QSizePolicy.newWithOptions policy policy QSizePolicy.DefaultType
