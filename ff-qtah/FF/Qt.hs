{-# LANGUAGE BlockArguments #-}

module FF.Qt (printChildrenTree, repeatInGuiThreadWheneverIdle, runInGuiThreadWhenReady) where

import Control.Concurrent.MVar (MVar, tryTakeMVar)
import Data.Foldable (for_)
import Graphics.UI.Qtah.Core.QMetaClassInfo qualified as QMetaClassInfo
import Graphics.UI.Qtah.Core.QMetaObject qualified as QMetaObject
import Graphics.UI.Qtah.Core.QObject (QObjectPtr, toQObject)
import Graphics.UI.Qtah.Core.QObject qualified as QObject
import Graphics.UI.Qtah.Core.QTimer qualified as QTimer
import Graphics.UI.Qtah.Signal (connect_)

printChildrenTree :: (QObjectPtr object) => object -> IO ()
printChildrenTree = go 0 . toQObject
  where
    go level object = do
        name <- QObject.objectName object
        meta <- QObject.metaObject object
        classInfo <- QMetaObject.classInfo meta 0
        className <- QMetaClassInfo.name classInfo
        putStrLn . unwords $
            replicate level "| " ++ [show name, ":", show className]
        children <- QObject.children object
        for_ children $ go (level + 1)

-- | Repaeat some code in the GUI thread, when it is idle
repeatInGuiThreadWheneverIdle :: IO () -> IO ()
repeatInGuiThreadWheneverIdle action = do
    t <- QTimer.new
    connect_ t QTimer.timeoutSignal action
    QTimer.start t 0

-- | When an MVar gets a value, run the handler once in the GUI thread.
runInGuiThreadWhenReady :: MVar a -> (a -> IO ()) -> IO ()
runInGuiThreadWhenReady var action = do
    t <- QTimer.new
    connect_ t QTimer.timeoutSignal do
        mVal <- tryTakeMVar var
        for_ mVal \val -> do
            QObject.deleteLater t
            action val
    QTimer.start t 0
