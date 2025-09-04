{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

import Brick (
    App (App),
    AttrMap,
    AttrName,
    BrickEvent (VtyEvent),
    EventM,
    Widget,
    attrMap,
    attrName,
    defaultMain,
    halt,
    neverShowCursor,
    on,
    str,
    withAttr,
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

main :: IO ()
main = void $ defaultMain app initialModel

newtype Model = Model {notes :: List () String} deriving (Generic)

initialModel :: Model
initialModel =
    Model
        { notes =
            list
                ()
                ( Vector.fromList
                    ["alpha", "beta", "gamma", "delta", "epsilon", "zeta"]
                )
                4
        }

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
    [ borderWithLabel (str "Agenda") (renderList renderListItem True notes)
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
