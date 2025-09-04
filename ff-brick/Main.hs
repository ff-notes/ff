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
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty (Event (EvKey), Key (KEsc), black, defAttr, white)
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as StorageFS

import FF (getDataDir, loadAllNotes, noDataDirectoryMessage)
import FF.Config (loadConfig)

newtype Model = Model {notes :: List () String} deriving (Generic)

main :: IO ()
main = do
    cfg <- loadConfig
    dataDir <- getDataDir cfg
    handleM <- traverse StorageFS.newHandle dataDir
    handle <- handleM `orElse` fail noDataDirectoryMessage
    notes <- runStorage handle loadAllNotes
    let initialModel =
            Model
                { notes =
                    list () (Vector.fromList $ map show notes) listItemHeight
                }
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
        (withVScrollBars OnRight $ renderList renderListItem True notes)
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

renderListItem :: Bool -> String -> Widget ()
renderListItem _isSelected = str
