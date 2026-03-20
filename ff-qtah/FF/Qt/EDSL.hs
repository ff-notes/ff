{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module FF.Qt.EDSL where

import Data.Foldable (for_)
import Graphics.UI.Qtah.Core.QObject qualified as QObject
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Widgets.QAbstractButton qualified as QAbstractButton
import Graphics.UI.Qtah.Widgets.QBoxLayout (QBoxLayoutPtr)
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFormLayout qualified as QFormLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QHBoxLayout qualified as QHBoxLayout
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QLayout (QLayoutPtr, toQLayout)
import Graphics.UI.Qtah.Widgets.QPushButton (QPushButton)
import Graphics.UI.Qtah.Widgets.QPushButton qualified as QPushButton
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy)
import Graphics.UI.Qtah.Widgets.QVBoxLayout qualified as QVBoxLayout
import Graphics.UI.Qtah.Widgets.QWidget (QWidgetPtr, toQWidget)
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import Named (NamedF (ArgF), (:?))

data QBoxLayoutItem
    = Stretch
    | forall a. (QWidgetPtr a) => Widget (IO a)

data QFormLayoutItem
    = forall a. (QLayoutPtr a) => RowLayout (IO a)
    | forall a. (QWidgetPtr a) => RowWidget (IO a)
    | forall a. (QLayoutPtr a) => StringLayout String a
    | forall a. (QWidgetPtr a) => StringWidget String (IO a)

data Layout
    = QFormLayout [QFormLayoutItem]
    | QVBoxLayout [QBoxLayoutItem]

qFrame :: "objectName" :? String -> Layout -> IO QFrame
qFrame (ArgF objectName) lo = do
    obj <- QFrame.new
    for_ objectName $ QObject.setObjectName obj
    case lo of
        QFormLayout items -> do
            form <- QFormLayout.newWithParent obj
            for_ objectName \oname ->
                QObject.setObjectName form $ oname <> ".form"
            for_ items $ addRow form
        QVBoxLayout items -> do
            box <- QVBoxLayout.newWithParent obj
            for_ objectName \oname ->
                QObject.setObjectName box $ oname <> ".box"
            for_ items $ addBoxLayoutItem box
    pure obj
  where
    addRow form = \case
        RowLayout a -> QFormLayout.addRowLayout form . toQLayout =<< a
        RowWidget a -> QFormLayout.addRowWidget form . toQWidget =<< a
        StringLayout s a -> QFormLayout.addRowStringLayout form s $ toQLayout a
        StringWidget s a ->
            QFormLayout.addRowStringWidget form s . toQWidget =<< a

addBoxLayoutItem :: (QBoxLayoutPtr p) => p -> QBoxLayoutItem -> IO ()
addBoxLayoutItem box = \case
    Stretch -> QBoxLayout.addStretch box
    Widget io -> QBoxLayout.addWidget box =<< io

hline :: IO QFrame
hline = do
    obj <- QFrame.new
    QFrame.setFrameShape obj QFrame.HLine
    pure obj

qHBoxLayout ::
    "objectName" :? String ->
    "spacing" :? Int ->
    [QBoxLayoutItem] ->
    IO QHBoxLayout
qHBoxLayout (ArgF objectName) (ArgF spacing) items = do
    obj <- QHBoxLayout.new
    for_ objectName $ QObject.setObjectName obj
    for_ spacing $ QBoxLayout.setSpacing obj
    for_ items $ addBoxLayoutItem obj
    pure obj

qLabel ::
    "alignment" :? Qt.QtAlignmentFlag ->
    "objectName" :? String ->
    "openExternalLinks" :? Bool ->
    "sizePolicy" :? (QSizePolicyPolicy, QSizePolicyPolicy) ->
    "text" :? String ->
    "textFormat" :? Qt.QtTextFormat ->
    "textInteractionFlags" :? Qt.QtTextInteractionFlags ->
    "wordWrap" :? Bool ->
    IO QLabel
qLabel
    (ArgF alignment)
    (ArgF objectName)
    (ArgF openExternalLinks)
    (ArgF sizePolicy)
    (ArgF text)
    (ArgF textFormat)
    (ArgF textInteractionFlags)
    (ArgF wordWrap) = do
        obj <- QLabel.new
        for_ alignment $ QLabel.setAlignment obj
        for_ objectName $ QObject.setObjectName obj
        for_ openExternalLinks $ QLabel.setOpenExternalLinks obj
        for_ sizePolicy \(sp1, sp2) -> QWidget.setSizePolicyRaw obj sp1 sp2
        for_ text $ QLabel.setText obj
        for_ textFormat $ QLabel.setTextFormat obj
        for_ textInteractionFlags $ QLabel.setTextInteractionFlags obj
        for_ wordWrap $ QLabel.setWordWrap obj
        pure obj

qPushButton :: "objectName" :? String -> "text" :? String -> IO QPushButton
qPushButton (ArgF objectName) (ArgF text) = do
    obj <- QPushButton.new
    for_ objectName $ QObject.setObjectName obj
    for_ text $ QAbstractButton.setText obj
    pure obj

qScrollArea :: (QWidgetPtr widget) => widget -> IO QScrollArea
qScrollArea w = do
    obj <- QScrollArea.new
    QScrollArea.setWidget obj w
    QScrollArea.setWidgetResizable obj True
    pure obj
