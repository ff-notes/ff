{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskListWidget (
  TaskListWidget, getNoteId, getTitle, new, setDebugInfoVisible, upsertTask
) where

-- global
import           Control.Monad (void)
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           RON.Storage.Backend (DocId (DocId))

-- project
import           FF (fromRgaM)
import           FF.Types (Entity (..), EntityView, Note (..),
                           View (NoteView, note))

type TaskListWidget = QTreeWidget

data Column = NoteIdColumn | TitleColumn | ColumnCount deriving Enum

getNoteId, getTitle :: QTreeWidgetItem -> IO String
getNoteId item = QTreeWidgetItem.text item $ fromEnum NoteIdColumn
getTitle  item = QTreeWidgetItem.text item $ fromEnum TitleColumn

new :: IO TaskListWidget
new = do
  this <- QTreeWidget.new
  QAbstractItemView.setAlternatingRowColors this True
  QTreeView.setHeaderHidden                 this True
  QTreeWidget.setColumnCount                this $ fromEnum ColumnCount
  setDebugInfoVisible                       this False
  pure this

setDebugInfoVisible :: TaskListWidget -> Bool -> IO ()
setDebugInfoVisible this =
  QTreeView.setColumnHidden this (fromEnum NoteIdColumn) . not

-- Only insertion is implemeted. TODO implement update.
upsertTask :: TaskListWidget -> EntityView Note -> IO ()
upsertTask tree Entity{entityId, entityVal = noteView} =
  void $ QTreeWidgetItem.newWithParentTreeAndStrings tree [noteId, title]
  where
    DocId noteId = entityId
    NoteView{note} = noteView
    Note{note_text} = note
    text = fromRgaM note_text
    title = concat $ take 1 $ lines text
