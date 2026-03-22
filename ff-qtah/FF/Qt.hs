{-# LANGUAGE BlockArguments #-}

module FF.Qt (
    hDateFormat,
    qDateFormat,
    repeatInGuiThreadWheneverIdle,
    runInGuiThreadWhenReady,
) where

import Control.Concurrent.MVar (MVar, tryTakeMVar)
import Data.Foldable (for_)
import Graphics.UI.Qtah.Core.QObject qualified as QObject
import Graphics.UI.Qtah.Core.QTimer qualified as QTimer
import Graphics.UI.Qtah.Signal (connect_)

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

qDateFormat :: String
qDateFormat = "ddd, d MMM yyyy"

hDateFormat :: String
hDateFormat = "%a, %-e %b %Y"
