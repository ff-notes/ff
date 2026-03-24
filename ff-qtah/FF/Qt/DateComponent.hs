{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Qt.DateComponent (DateComponent (..), new, setDate) where

import Data.Time (Day, toGregorian)
import Foreign.Hoppy.Runtime (toGc)
import Graphics.UI.Qtah.Core.QDate qualified as QDate
import Graphics.UI.Qtah.Widgets.QAbstractSpinBox qualified as QAbstractSpinBox
import Graphics.UI.Qtah.Widgets.QDateEdit (QDateEdit)
import Graphics.UI.Qtah.Widgets.QDateTimeEdit qualified as QDateTimeEdit
import Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import Graphics.UI.Qtah.Widgets.QPushButton (QPushButton)
import Graphics.UI.Qtah.Widgets.QWidget qualified as QWidget
import Named (defaults, (!))

import FF.Qt (qDateFormat)
import FF.Qt.EDSL (
    QBoxLayoutItem (Widget),
    qDateEdit,
    qHBoxLayout,
    qPushButton,
    ($<),
 )

data DateComponent = DateComponent
    { parent :: QHBoxLayout
    , date :: QDateEdit
    , add :: QPushButton
    , remove :: QPushButton
    }

new :: IO DateComponent
new = do
    date <-
        qDateEdit
            -- ! #calendarPopup True -- TODO bad styling on Mac
            ! #displayFormat qDateFormat
    add <- qPushButton ! #objectName "set" ! #text "➕ Set" ! defaults
    remove <- qPushButton ! #objectName "remove" ! #text "╳" ! defaults
    parent <-
        qHBoxLayout ! #objectName "[DateComponent]parent" ! #spacing 0 $
            [Widget $< date, Widget $< add, Widget $< remove]
    let this = DateComponent{parent, date, add, remove}
    setEditable this False
    pure this

setEditable :: DateComponent -> Bool -> IO ()
setEditable this editable = do
    QAbstractSpinBox.setReadOnly this.date $ not editable
    QWidget.setEnabled this.add editable
    QWidget.setEnabled this.remove editable

setDate :: DateComponent -> Maybe Day -> IO ()
setDate this day = do
    dayIsSet <-
        case day of
            Just (toGregorian -> (y, m, d)) -> do
                qdate <- toGc =<< QDate.newWithYmd (fromInteger y) m d
                QDateTimeEdit.setDate this.date qdate
                pure True
            Nothing ->
                pure False
    QWidget.setVisible this.date dayIsSet
    QWidget.setVisible this.remove dayIsSet
    QWidget.setVisible this.add $ not dayIsSet
