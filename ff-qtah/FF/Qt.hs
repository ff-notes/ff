module FF.Qt (printChildrenTree, whenUIIdle) where

import           Data.Foldable (for_)
import qualified Graphics.UI.Qtah.Core.QMetaClassInfo as QMetaClassInfo
import qualified Graphics.UI.Qtah.Core.QMetaObject as QMetaObject
import           Graphics.UI.Qtah.Core.QObject (QObjectPtr, toQObject)
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import           Graphics.UI.Qtah.Signal (connect_)

printChildrenTree :: QObjectPtr object => object -> IO ()
printChildrenTree = go 0 . toQObject where
  go level object = do
    name <- QObject.objectName object
    meta <- QObject.metaObject object
    classInfo <- QMetaObject.classInfo meta 0
    className <- QMetaClassInfo.name classInfo
    putStrLn $
      unwords $ replicate level "| " ++ [show name, ":", show className]
    children <- QObject.children object
    for_ children $ go (level + 1)

whenUIIdle :: IO () -> IO ()
whenUIIdle action = do
  t <- QTimer.new
  connect_ t QTimer.timeoutSignal action
  QTimer.start t 0
