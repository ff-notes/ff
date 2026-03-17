{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Qt.DateComponent (DateComponent (super), new, setDate) where

import Data.Time (Day, toGregorian)
import Graphics.UI.Qtah.Core.QDate qualified as QDate
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
    QDateTimeEdit.setDisplayFormat dateEdit "ddd d MMM yyyy"
    QBoxLayout.addWidget super dateEdit

    QBoxLayout.addStretch super

    let this = DateComponent{super, label, dateEdit}
    setEditable this False
    pure this

setEditable :: DateComponent -> Bool -> IO ()
setEditable DateComponent{dateEdit} editable =
    QAbstractSpinBox.setReadOnly dateEdit $ not editable

setDate :: DateComponent -> Maybe Day -> IO ()
setDate DateComponent{dateEdit} day = do
    qdate <-
        case day of
            Just (toGregorian -> (y, m, d)) ->
                QDate.newWithYmd (fromInteger y) m d
            Nothing -> QDate.new -- TODO replace with button "add date"
    QDateTimeEdit.setDate dateEdit qdate
