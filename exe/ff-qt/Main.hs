module Main (main) where

import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import           Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow,
                                                       setCentralWidget)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import           Graphics.UI.Qtah.Widgets.QTabWidget (addTab)
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- mkMainWindow
    QWidget.show mainWindow
    exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ getArgs >>= QApplication.new

mkMainWindow :: IO QMainWindow
mkMainWindow = do
    mainWindow <- QMainWindow.new
    setCentralWidget mainWindow =<< do
        tabs <- QTabWidget.new
        agenda <- mkAgendaWidget
        _ <- addTab tabs agenda "Agenda"
        pure tabs
    pure mainWindow

mkAgendaWidget :: IO QTreeWidget
mkAgendaWidget = do
    agenda <- QTreeWidget.new
    setHeaderHidden agenda True
    pure agenda
