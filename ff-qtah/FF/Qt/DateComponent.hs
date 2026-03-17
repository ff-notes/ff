{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Qt.DateComponent (DateComponent (..), new, setDate) where

import Data.Time (Day, toGregorian)
import Graphics.UI.Qtah.Core.QDate qualified as QDate
import Graphics.UI.Qtah.Widgets.QAbstractSpinBox qualified as QAbstractSpinBox
import Graphics.UI.Qtah.Widgets.QBoxLayout qualified as QBoxLayout
import Graphics.UI.Qtah.Widgets.QDateEdit (QDateEdit)
import Graphics.UI.Qtah.Widgets.QDateEdit qualified as QDateEdit
import Graphics.UI.Qtah.Widgets.QDateTimeEdit qualified as QDateTimeEdit
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QHBoxLayout qualified as QHBoxLayout
import Graphics.UI.Qtah.Widgets.QPushButton (QPushButton)
import Graphics.UI.Qtah.Widgets.QPushButton qualified as QPushButton
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget

data DateComponent = DateComponent
    { box :: QHBoxLayout
    , date :: QDateEdit
    , add :: QPushButton
    , remove :: QPushButton
    }

new :: IO DateComponent
new = do
    box <- QHBoxLayout.new

    date <- QDateEdit.new
    QDateTimeEdit.setCalendarPopup date True
    QDateTimeEdit.setDisplayFormat date "ddd d MMM yyyy"
    QBoxLayout.addWidget box date

    add <- QPushButton.newWithText "+"
    QWidget.setEnabled add False
    QBoxLayout.addWidget box add

    remove <- QPushButton.newWithText "−"
    QWidget.setEnabled remove False
    QBoxLayout.addWidget box remove

    let this = DateComponent{box, date, add, remove}
    setEditable this False
    pure this

setEditable :: DateComponent -> Bool -> IO ()
setEditable DateComponent{date} editable =
    QAbstractSpinBox.setReadOnly date $ not editable

setDate :: DateComponent -> Maybe Day -> IO ()
setDate this day =
    case day of
        Just (toGregorian -> (y, m, d)) -> do
            QWidget.show this.date
            QWidget.hide this.add
            QWidget.show this.remove
            qdate <- QDate.newWithYmd (fromInteger y) m d
            QDateTimeEdit.setDate this.date qdate
        Nothing -> do
            -- TODO replace with button "add date"
            QWidget.hide this.date
            QWidget.show this.add
            QWidget.hide this.remove
