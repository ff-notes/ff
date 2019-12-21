{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.DateComponent (DateComponent, new) where

-- global
import           Foreign (castPtr)
import           Foreign.Hoppy.Runtime (CppPtr, nullptr, toPtr, touchCppPtr,
                                        withCppPtr)
import           Graphics.UI.Qtah.Core.QObject (QObjectConstPtr, QObjectPtr,
                                                toQObject, toQObjectConst)
import qualified Graphics.UI.Qtah.Widgets.QAbstractSpinBox as QAbstractSpinBox
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import           Graphics.UI.Qtah.Widgets.QDateEdit (QDateEdit)
import qualified Graphics.UI.Qtah.Widgets.QDateEdit as QDateEdit
import qualified Graphics.UI.Qtah.Widgets.QDateTimeEdit as QDateTimeEdit
import           Graphics.UI.Qtah.Widgets.QHBoxLayout (QHBoxLayout)
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import           Graphics.UI.Qtah.Widgets.QLayout (QLayoutConstPtr, QLayoutPtr,
                                                   toQLayout, toQLayoutConst)
import           Graphics.UI.Qtah.Widgets.QLayoutItem (QLayoutItemConstPtr,
                                                       QLayoutItemPtr,
                                                       toQLayoutItem,
                                                       toQLayoutItemConst)

data DateComponent =
  DateComponent{super :: QHBoxLayout, label :: QLabel, dateEdit :: QDateEdit}

instance CppPtr DateComponent where
  nullptr = DateComponent{super = nullptr, label = nullptr, dateEdit = nullptr}
  withCppPtr DateComponent{super} proc = withCppPtr super $ proc . castPtr
  toPtr = castPtr . toPtr . super
  touchCppPtr = touchCppPtr . super

instance QObjectConstPtr DateComponent where
  toQObjectConst = toQObjectConst . super

instance QObjectPtr DateComponent where
  toQObject = toQObject . super

instance QLayoutItemConstPtr DateComponent where
  toQLayoutItemConst = toQLayoutItemConst . super

instance QLayoutItemPtr DateComponent where
  toQLayoutItem = toQLayoutItem . super

instance QLayoutConstPtr DateComponent where
  toQLayoutConst = toQLayoutConst . super

instance QLayoutPtr DateComponent where
  toQLayout = toQLayout . super

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
