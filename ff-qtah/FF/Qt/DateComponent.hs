{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.DateComponent (DateComponent (super), new) where

-- global
import Graphics.UI.Qtah.Widgets.QAbstractSpinBox qualified as QAbstractSpinBox
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QDateEdit (QDateEdit)
import Graphics.UI.Qtah.Widgets.QDateEdit qualified as QDateEdit
import Graphics.UI.Qtah.Widgets.QDateTimeEdit qualified as QDateTimeEdit
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QHBoxLayout qualified as QHBoxLayout
import Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import Graphics.UI.Qtah.Widgets.QLabel qualified as QLabel

data DateComponent = DateComponent
    { super :: QHBoxLayout
    , label :: QLabel
    , dateEdit :: QDateEdit
    }

new :: String -> IO DateComponent
new title = do
    super <- QHBoxLayout.new

    label <- QLabel.newWithText title
    QBoxLayout.addWidget super label

    dateEdit <- QDateEdit.new
    QDateTimeEdit.setCalendarPopup dateEdit True
    QBoxLayout.addWidget super dateEdit

    QBoxLayout.addStretch super

    let this = DateComponent{super, label, dateEdit}
    setEditable this False
    pure this

setEditable :: DateComponent -> Bool -> IO ()
setEditable DateComponent{dateEdit} editable =
    QAbstractSpinBox.setReadOnly dateEdit $ not editable
