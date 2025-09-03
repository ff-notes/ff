{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

import Brick (
    App (App),
    AttrMap,
    BrickEvent (VtyEvent),
    EventM,
    Widget,
    attrMap,
    defaultMain,
    halt,
    neverShowCursor,
    on,
    str,
    withAttr,
    zoom,
    (<=>),
 )
import Brick qualified
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listSelectedAttr,
    renderList,
 )
import Control.Monad (void)
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty (Event (EvKey), Key (KEsc), black, defAttr, white)

main :: IO ()
main = void $ defaultMain app initialModel

newtype Model = Model {notes :: List () String} deriving (Generic)

initialModel :: Model
initialModel =
    Model{notes = list () (Vector.fromList ["3", "15", "9", "20"]) 10}

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
appAttrMap = attrMap defAttr [(listSelectedAttr, black `on` white)]

appDraw :: Model -> [Widget ()]
appDraw Model{notes} =
    [ borderWithLabel (str "Agenda") (renderList renderListItem True notes)
        <=> str "Esc - exit"
    ]

appHandleEvent :: BrickEvent () () -> EventM () Model ()
appHandleEvent = \case
    VtyEvent e -> appHandleVtyEvent e
    _ -> pure ()

appHandleVtyEvent :: Event -> EventM () Model ()
appHandleVtyEvent = \case
    EvKey KEsc _ -> halt
    e -> zoom #notes $ handleListEvent e

renderListItem :: Bool -> String -> Widget ()
renderListItem isSelected item =
    (if isSelected then withAttr listSelectedAttr else id) $ str item
