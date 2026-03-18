{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Qt.EDSL where

import Data.Foldable (for_)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QFormLayout qualified as QFormLayout
import Graphics.UI.Qtah.Widgets.QFrame (QFrame)
import Graphics.UI.Qtah.Widgets.QFrame qualified as QFrame
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QHBoxLayout qualified as QHBoxLayout
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel
import Graphics.UI.Qtah.Widgets.QLayout (QLayoutPtr (toQLayout))
import Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import Graphics.UI.Qtah.Widgets.QScrollArea qualified as QScrollArea
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy)
import Graphics.UI.Qtah.Widgets.QWidget (QWidgetPtr, toQWidget)
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import Named (arg, (:!))

data QBoxLayoutItem
    = Stretch
    | forall a. (QWidgetPtr a) => Widget (IO a)

data QFormLayoutItem
    = forall a. (QLayoutPtr a) => RowLayout (IO a)
    | forall a. (QWidgetPtr a) => RowWidget (IO a)
    | forall a. (QLayoutPtr a) => StringLayout String a

-- newtype Layout = QFormLayout [QFormLayoutItem]

qFrame :: [QFormLayoutItem] -> IO QFrame
qFrame items = do
    obj <- QFrame.new
    -- case lo of
    --     QFormLayout items -> do
    form <- QFormLayout.newWithParent obj
    for_ items $ addRow form
    pure obj
  where
    addRow form = \case
        RowLayout io -> QFormLayout.addRowLayout form . toQLayout =<< io
        RowWidget io -> QFormLayout.addRowWidget form . toQWidget =<< io
        StringLayout s c -> QFormLayout.addRowStringLayout form s $ toQLayout c

hline :: IO QFrame
hline = do
    obj <- QFrame.new
    QFrame.setFrameShape obj QFrame.HLine
    pure obj

qHBoxLayout :: [QBoxLayoutItem] -> IO QHBoxLayout
qHBoxLayout items = do
    obj <- QHBoxLayout.new
    for_ items \case
        Stretch -> QBoxLayout.addStretch obj
        Widget io -> QBoxLayout.addWidget obj =<< io
    pure obj

qLabel ::
    (Qt.IsQtTextInteractionFlags textInteractionFlags) =>
    "alignment" :! Qt.QtAlignmentFlag ->
    "openExternalLinks" :! Bool ->
    "sizePolicy" :! (QSizePolicyPolicy, QSizePolicyPolicy) ->
    "textInteractionFlags" :! textInteractionFlags ->
    "textFormat" :! Qt.QtTextFormat ->
    "wordWrap" :! Bool ->
    IO QLabel
qLabel
    (arg #alignment -> a)
    (arg #openExternalLinks -> oel)
    (arg #sizePolicy -> (sp1, sp2))
    (arg #textInteractionFlags -> tif)
    (arg #textFormat -> tf)
    (arg #wordWrap -> ww) = do
        obj <- QLabel.new
        QLabel.setAlignment obj a
        QLabel.setOpenExternalLinks obj oel
        QWidget.setSizePolicyRaw obj sp1 sp2
        QLabel.setTextInteractionFlags obj tif
        QLabel.setTextFormat obj tf
        QLabel.setWordWrap obj ww
        pure obj

qScrollArea :: (QWidgetPtr widget) => widget -> IO QScrollArea
qScrollArea w = do
    obj <- QScrollArea.new
    QScrollArea.setWidget obj w
    pure obj
