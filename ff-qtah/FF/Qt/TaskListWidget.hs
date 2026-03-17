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

import Control.Monad (void)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Foreign.Hoppy.Runtime (fromCppEnum, toGc)
import Graphics.UI.Qtah.Core.Types qualified as Qt
import Graphics.UI.Qtah.Gui.QFont (QFont)
import Graphics.UI.Qtah.Gui.QFont qualified as QFont
import Graphics.UI.Qtah.Widgets.QAbstractItemView qualified as QAbstractItemView
import Graphics.UI.Qtah.Widgets.QTreeView qualified as QTreeView
import Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import Graphics.UI.Qtah.Widgets.QTreeWidget qualified as QTreeWidget
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import Graphics.UI.Qtah.Widgets.QTreeWidgetItem qualified as QTreeWidgetItem
import RON.Storage.Backend (DocId (DocId))
import Text.Printf (printf)

import FF (fromRgaM)
import FF.Types (
    Entity (..),
    EntityView,
    Note (..),
    TaskMode (..),
    View (NoteView, note),
    taskMode,
 )
import FF.UI (sampleLabel)

data TaskListWidget = TaskListWidget
    {parent :: QTreeWidget, modeItems :: IORef (Map TaskMode QTreeWidgetItem)}

{- | Value order in this enumeration defines the field order in the tree widget.
0th column mustn't be hideable, because when 0th column is hidden,
the tree structure, alternating row color, and child indicators
are hidden too.
-}
data Field
    = TitleField
    | IdField
    | -- | see NaturalTaskOrder.md
      SortKeyField
    deriving (Bounded, Enum)

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
    QTreeWidget.setHeaderLabels parent $
        fieldsToStrings \case
            TitleField -> "Title"
            IdField -> "UUID"
            SortKeyField -> "Sort key"
    -- columns must be defined before sorting
    QTreeView.setSortingEnabled parent True
    QTreeWidget.sortItems parent (fromEnum SortKeyField) Qt.AscendingOrder

    modeItems <- newIORef mempty

    let this = TaskListWidget{parent, modeItems}

    setDebugInfoVisible this False

    pure this

setDebugInfoVisible :: TaskListWidget -> Bool -> IO ()
setDebugInfoVisible this v = do
    QTreeView.setColumnHidden this.parent (fromEnum IdField) $ not v
    QTreeView.setColumnHidden this.parent (fromEnum SortKeyField) $ not v
    QTreeView.setHeaderHidden this.parent $ not v

-- Only insertion is implemeted. TODO implement update.
upsertTask :: TaskListWidget -> EntityView Note -> IO ()
upsertTask this entity = do
    today <- utctDay <$> getCurrentTime
    let mode = taskMode today note
    mModeItem <- Map.lookup mode <$> readIORef this.modeItems
    modeItem <- case mModeItem of
        Just item -> pure item
        Nothing -> createModeItem this mode
    createTaskItem modeItem entity
  where
    Entity{entityVal = NoteView{note}} = entity

createModeItem :: TaskListWidget -> TaskMode -> IO QTreeWidgetItem
createModeItem this mode = do
    item <-
        QTreeWidgetItem.newWithParentTreeAndStringsAndType
            this.parent
            ( fieldsToStrings \case
                IdField -> show mode
                SortKeyField ->
                    case mode of
                        -- 999_999 days ~ 2700 years, should be enough
                        Overdue n -> printf "0Overdue-%06d" (999_999 - n)
                        EndToday -> printf "1EndToday"
                        EndSoon n -> printf "2EndSoon+%06d" n
                        Actual -> printf "3Actual"
                        Starting n -> printf "4Starting+%06d" n
                TitleField -> Text.unpack $ sampleLabel mode
            )
            (itemTypeToInt ModeGroup)
    QTreeWidgetItem.setExpanded item True
    QTreeWidgetItem.setFont item (fromEnum TitleField) =<< makeBoldFont
    modifyIORef this.modeItems $ Map.insert mode item
    pure item

createTaskItem :: QTreeWidgetItem -> EntityView Note -> IO ()
createTaskItem modeItem entity =
    void $
        QTreeWidgetItem.newWithParentItemAndStringsAndType
            modeItem
            ( fieldsToStrings \case
                IdField -> noteId
                SortKeyField -> sortKey
                TitleField -> title
            )
            (itemTypeToInt Task)
  where
    Entity{entityId = DocId noteId, entityVal = NoteView{note}} = entity
    title = concat $ take 1 $ lines $ fromRgaM note.note_text
    sortKey = printf "End=%04d%02d%02d,Start=%04d%02d%02d" ey em ed sy sm sd
    (ey, em, ed) = maybe (9999, 99, 99) toGregorian note.note_end
    (sy, sm, sd) = maybe (0, 0, 0) toGregorian note.note_start

makeBoldFont :: IO QFont
makeBoldFont = do
    font <- toGc =<< QFont.new
    QFont.setBold font True
    pure font
