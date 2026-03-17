{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskWidget (
    TaskWidget,
    new,
    update,
) where

-- global
import Foreign (castPtr)
import Foreign.Hoppy.Runtime (
    CppPtr,
    nullptr,
    toGc,
    toPtr,
    touchCppPtr,
    withCppPtr,
 )
import Graphics.UI.Qtah.Core.QObject (
    QObjectConstPtr,
    QObjectPtr,
    toQObject,
    toQObjectConst,
 )
import Graphics.UI.Qtah.Core.Types (QtAlignmentFlag (AlignTop))
import Graphics.UI.Qtah.Gui.QPaintDevice (
    QPaintDeviceConstPtr,
    QPaintDevicePtr,
    toQPaintDevice,
    toQPaintDeviceConst,
 )
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (
    QSizePolicy,
    QSizePolicyPolicy,
 )
import Graphics.UI.Qtah.Widgets.QSizePolicy qualified as QSizePolicy
import Graphics.UI.Qtah.Widgets.QVBoxLayout qualified as QVBoxLayout
import Graphics.UI.Qtah.Widgets.QWidget (
    QWidgetConstPtr,
    QWidgetPtr,
    toQWidget,
    toQWidgetConst,
 )
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

instance CppPtr TaskWidget where
    nullptr =
        TaskWidget
            { super = nullptr
            , frame = nullptr
            , label = nullptr
            , storage = undefined
            }
    withCppPtr TaskWidget{super} proc = withCppPtr super $ proc . castPtr
    toPtr = castPtr . toPtr . super
    touchCppPtr = touchCppPtr . super

instance QObjectConstPtr TaskWidget where
    toQObjectConst = toQObjectConst . super

instance QObjectPtr TaskWidget where
    toQObject = toQObject . super

instance QPaintDeviceConstPtr TaskWidget where
    toQPaintDeviceConst = toQPaintDeviceConst . super

instance QPaintDevicePtr TaskWidget where
    toQPaintDevice = toQPaintDevice . super

instance QWidgetConstPtr TaskWidget where
    toQWidgetConst = toQWidgetConst . super

instance QWidgetPtr TaskWidget where
    toQWidget = toQWidget . super

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
    QBoxLayout.addLayout box start
    QBoxLayout.addLayout box end

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
