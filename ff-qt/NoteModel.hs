{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module NoteModel
    ( NoteModel (..)
    , addNote
    , new
    ) where

import qualified Data.Text as Text
import           Graphics.UI.Qtah.Gui.QFont (setBold)
import qualified Graphics.UI.Qtah.Gui.QFont as QFont
import           Graphics.UI.Qtah.Gui.QStandardItem (QStandardItem, setFont)
import qualified Graphics.UI.Qtah.Gui.QStandardItem as QStandardItem
import           Graphics.UI.Qtah.Gui.QStandardItemModel (QStandardItemModel)
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel

import           FF (getUtcToday)
import           FF.Types (ModeMap (..), NoteView (NoteView, text), modeSelect,
                           taskMode)

-- TODO(cblp, 2018-03-02) QAbstractItemModel? without doubling in QStandardItemModel
data NoteModel = NoteModel
    { super        :: QStandardItemModel
    , modeSections :: ModeMap QStandardItem
    }

new :: IO NoteModel
new = do
    font <- QFont.new
    setBold font True

    super <- QStandardItemModel.new
    let newSection label = do
            item <- QStandardItem.newWithText label
            setFont                          item  font
            QStandardItemModel.appendRowItem super item
            pure item
    overdue  <- newSection "Overdue"
    endToday <- newSection "Due today"
    endSoon  <- newSection "Due soon"
    actual   <- newSection "Actual"
    starting <- newSection "Starting soon"
    let modeSections = ModeMap {..}
    pure NoteModel {..}

addNote :: NoteModel -> NoteView -> IO ()
addNote NoteModel { modeSections } note@NoteView { text } = do
    today <- getUtcToday
    item  <- QStandardItem.newWithText $ Text.unpack text
    let sectionItem = modeSelect modeSections $ taskMode today note
    QStandardItem.appendRowItem sectionItem item
