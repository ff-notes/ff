module FF.Qt (whenUIIdle) where

import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import           Graphics.UI.Qtah.Signal (connect_)

whenUIIdle :: IO () -> IO ()
whenUIIdle action = do
  t <- QTimer.new
  connect_ t QTimer.timeoutSignal action
  QTimer.start t 0
