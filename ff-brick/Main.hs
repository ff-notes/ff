{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

import Brick (
    App (App),
    AttrMap,
    AttrName,
    BrickEvent (VtyEvent),
    EventM,
    VScrollBarOrientation (OnRight),
    Widget,
    attrMap,
    attrName,
    defaultMain,
    halt,
    neverShowCursor,
    on,
    str,
    withAttr,
    withVScrollBars,
    zoom,
    (<+>),
    (<=>),
 )
import Brick qualified
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listSelectedAttr,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty (Event (EvKey), Key (KEsc), black, defAttr, white)
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as StorageFS

import FF (fromRgaM, getDataDir, loadAllNotes, noDataDirectoryMessage)
import FF.Config (loadConfig)
import FF.Types (Entity (Entity), EntityDoc, Note (Note))
import FF.Types qualified

newtype Model = Model {notes :: List () (EntityDoc Note)} deriving (Generic)

main :: IO ()
main = do
    cfg <- loadConfig
    dataDir <- getDataDir cfg
    handleM <- traverse StorageFS.newHandle dataDir
    handle <- handleM `orElse` fail noDataDirectoryMessage
    notes <- runStorage handle loadAllNotes
    let initialModel =
            Model{notes = list () (Vector.fromList notes) listItemHeight}
    void $ defaultMain app initialModel

listItemHeight :: Int
listItemHeight = 1

orElse :: (Applicative m) => Maybe a -> m a -> m a
orElse m n = maybe n pure m

app :: App Model () ()
app =
    App
        { appAttrMap = const appAttrMap
        , appChooseCursor = neverShowCursor
        , appDraw
        , appHandleEvent
        , appStartEvent = pure ()
        }

appAttrMap :: AttrMap
appAttrMap =
    attrMap
        defAttr
        [ (listSelectedAttr, black `on` white)
        , (listSelectedFocusedAttr, black `on` white) -- TODO `withStyle` bold?
        , (highlightAttr, black `on` white)
        ]

appDraw :: Model -> [Widget ()]
appDraw Model{notes} =
    [ borderWithLabel
        (str "Agenda")
        (renderList renderListItem True notes & withVScrollBars OnRight)
        <=> (withAttr highlightAttr (str "Esc") <+> str " exit")
    ]

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

appHandleEvent :: BrickEvent () () -> EventM () Model ()
appHandleEvent = \case
    VtyEvent e -> appHandleVtyEvent e
    _ -> pure ()

appHandleVtyEvent :: Event -> EventM () Model ()
appHandleVtyEvent = \case
    EvKey KEsc _ -> halt
    e -> zoom #notes $ handleListEvent e

renderListItem :: Bool -> EntityDoc Note -> Widget ()
renderListItem _isSelected Entity{entityVal = Note{note_text}} =
    str
        case textLines of
            [] -> "..."
            [singleLine] -> singleLine
            firstLine : _ -> firstLine <> "..."
  where
    textLines = filter (not . null) $ lines $ fromRgaM note_text
