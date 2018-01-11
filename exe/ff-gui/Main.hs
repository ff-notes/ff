module Main (main) where

import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import           Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import           Graphics.UI.Qtah.Widgets.QWidget (showMaximized)
import           System.Environment (getArgs)

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    showMaximized mainWindow
    exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ do
    args <- getArgs
    QApplication.new args

makeMainWindow :: IO QMainWindow
makeMainWindow = QMainWindow.new
