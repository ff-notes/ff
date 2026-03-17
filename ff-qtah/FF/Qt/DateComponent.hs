{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Qt.DateComponent (DateComponent (..), new, setDate) where

import Data.Time (Day, toGregorian)
import Graphics.UI.Qtah.Core.QDate qualified as QDate
import Graphics.UI.Qtah.Widgets.QAbstractSpinBox qualified as QAbstractSpinBox
import Graphics.UI.Qtah.Widgets.QDateEdit (QDateEdit)
import Graphics.UI.Qtah.Widgets.QDateEdit qualified as QDateEdit
import Graphics.UI.Qtah.Widgets.QDateTimeEdit qualified as QDateTimeEdit

newtype DateComponent = DateComponent {dateEdit :: QDateEdit}

new :: IO DateComponent
new = do
    dateEdit <- QDateEdit.new
    QDateTimeEdit.setCalendarPopup dateEdit True
    QDateTimeEdit.setDisplayFormat dateEdit "ddd d MMM yyyy"

    let this = DateComponent{dateEdit}
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
