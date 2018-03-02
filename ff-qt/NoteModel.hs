{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module NoteModel
    ( NoteModel
    , addNote
    , new
    ) where

import qualified Data.Text as Text
import           Foreign.Hoppy.Runtime (CppPtr)
import           Graphics.UI.Qtah.Core.QAbstractItemModel (QAbstractItemModelConstPtr,
                                                           QAbstractItemModelPtr)
import           Graphics.UI.Qtah.Core.QObject (QObjectConstPtr, QObjectPtr)
import qualified Graphics.UI.Qtah.Gui.QStandardItem as QStandardItem
import           Graphics.UI.Qtah.Gui.QStandardItemModel (QStandardItemModel, QStandardItemModelConstPtr,
                                                          QStandardItemModelPtr,
                                                          appendRowItem)
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel

import           FF.Types (NoteView (NoteView, text))

newtype NoteModel = NoteModel QStandardItemModel
    deriving
        ( CppPtr
        , QAbstractItemModelConstPtr
        , QAbstractItemModelPtr
        , QObjectConstPtr
        , QObjectPtr
        , QStandardItemModelConstPtr
        , QStandardItemModelPtr
        )

new :: IO NoteModel
new = NoteModel <$> QStandardItemModel.new

addNote :: NoteModel -> NoteView -> IO ()
addNote model NoteView { text } =
    appendRowItem model =<< QStandardItem.newWithText (Text.unpack text)
