{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module FF.Qt.TaskListWidget (
    ItemType (..),
    itemTypeFromInt,
    itemTypeToInt,
    TaskListWidget (parent),
    getId,
    getTitle,
    new,
    setDebugInfoVisible,
    upsertTask,
) where

-- global
import Control.Monad (void)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time (getCurrentTime, utctDay)
import Foreign.Hoppy.Runtime (fromCppEnum, toGc)
import Graphics.UI.Qtah.Gui.QFont (QFont)
import Graphics.UI.Qtah.Gui.QFont qualified as QFont
import Graphics.UI.Qtah.Widgets.QAbstractItemView qualified as QAbstractItemView
import Graphics.UI.Qtah.Widgets.QTreeView qualified as QTreeView
import Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import Graphics.UI.Qtah.Widgets.QTreeWidget qualified as QTreeWidget
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem qualified as QTreeWidgetItem
import RON.Storage.Backend (DocId (DocId))

-- project
import FF (fromRgaM)
import FF.Types (
    Entity (..),
    EntityView,
    Note (..),
    TaskMode,
    View (NoteView, note),
    taskMode,
 )
import FF.UI (sampleLabel)

data TaskListWidget = TaskListWidget
    {parent :: QTreeWidget, modeItems :: IORef (Map TaskMode QTreeWidgetItem)}

{- | Value order in this enumeration defines the field order in the tree widget.
0th column mustn't be hideable, because when 0th column is hidden,
the tree strcuture, alternating row color, and child indicators
are hidden too.
-}
data Field = TitleField | IdField deriving (Bounded, Enum)

fieldCount :: Int
fieldCount = fromEnum (maxBound :: Field) + 1

fieldsToStrings :: (Field -> String) -> [String]
fieldsToStrings f = map f [minBound .. maxBound]

data ItemType = ModeGroup | Task

itemTypeFromInt :: Int -> ItemType
itemTypeFromInt i = case i - userType of
    0 -> ModeGroup
    1 -> Task
    _ -> error $ "itemTypeFromInt @ItemType " <> show i

itemTypeToInt :: ItemType -> Int
itemTypeToInt t = userType + case t of ModeGroup -> 0; Task -> 1

-- | Int value of QTreeWidgetItem.UserType
userType :: Int
userType = fromIntegral $ fromCppEnum QTreeWidgetItem.UserType

getId, getTitle :: QTreeWidgetItem -> IO String
getId item = QTreeWidgetItem.text item $ fromEnum IdField
getTitle item = QTreeWidgetItem.text item $ fromEnum TitleField

new :: IO TaskListWidget
new = do
    parent <- QTreeWidget.new
    QAbstractItemView.setAlternatingRowColors parent True
    QTreeView.setHeaderHidden parent True
    QTreeWidget.setColumnCount parent fieldCount

    modeItems <- newIORef mempty

    let this = TaskListWidget{parent, modeItems}

    setDebugInfoVisible this False

    pure this

setDebugInfoVisible :: TaskListWidget -> Bool -> IO ()
setDebugInfoVisible this =
    QTreeView.setColumnHidden this.parent (fromEnum IdField) . not

-- Only insertion is implemeted. TODO implement update.
upsertTask :: TaskListWidget -> EntityView Note -> IO ()
upsertTask TaskListWidget{parent, modeItems} Entity{entityId, entityVal} = do
    today <- utctDay <$> getCurrentTime
    let mode = taskMode today note
    mModeItem <- Map.lookup mode <$> readIORef modeItems
    modeItem <- case mModeItem of
        Just item ->
            pure item
        Nothing -> do
            item <-
                QTreeWidgetItem.newWithParentTreeAndStringsAndType
                    parent
                    ( fieldsToStrings \case
                        IdField -> show mode
                        TitleField -> Text.unpack $ sampleLabel mode
                    )
                    (itemTypeToInt ModeGroup)
            QTreeWidgetItem.setExpanded item True
            QTreeWidgetItem.setFont item (fromEnum TitleField) =<< makeBoldFont
            modifyIORef modeItems $ Map.insert mode item
            pure item
    void $
        QTreeWidgetItem.newWithParentItemAndStringsAndType
            modeItem
            (fieldsToStrings \case IdField -> noteId; TitleField -> title)
            (itemTypeToInt Task)
  where
    DocId noteId = entityId
    NoteView{note} = entityVal
    Note{note_text} = note
    text = fromRgaM note_text
    title = concat $ take 1 $ lines text

makeBoldFont :: IO QFont
makeBoldFont = do
    font <- toGc =<< QFont.new
    QFont.setBold font True
    pure font
