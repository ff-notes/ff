{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

import Brick (
    AttrMap,
    AttrName,
    VScrollBarOrientation (OnRight),
    ViewportType (Both),
    attrMap,
    attrName,
    defaultMain,
    hBox,
    halt,
    neverShowCursor,
    on,
    str,
    viewport,
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
    listSelectedElementL,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty (
    Event (EvKey),
    Key (KChar, KEnter, KEsc),
    black,
    defAttr,
    white,
 )
import Lens.Micro.Mtl (preuse, (.=))
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as StorageFS

import FF (fromRgaM, getDataDir, loadAllNotes, noDataDirectoryMessage)
import FF.Config (loadConfig)
import FF.Types (Entity (Entity), EntityDoc, Note (Note))
import FF.Types qualified

-- | Brick widget names
data WN
    = NoteList
    | OpenNoteViewport
    deriving (Eq, Ord, Show)

data Model = Model
    { notes :: List WN (EntityDoc Note)
    -- ^ all notes
    , openNoteM :: Maybe (EntityDoc Note)
    -- ^ currently opened note
    }
    deriving (Generic)

type Widget = Brick.Widget WN

type App = Brick.App Model () WN

type EventM = Brick.EventM WN Model

type BrickEvent = Brick.BrickEvent WN ()

main :: IO ()
main = do
    cfg <- loadConfig
    dataDirM <- getDataDir cfg
    handleM <- traverse StorageFS.newHandle dataDirM
    handle <- handleM `orElse` fail noDataDirectoryMessage
    notes <- runStorage handle loadAllNotes
    let initialModel =
            Model
                { notes = list NoteList (Vector.fromList notes) listItemHeight
                , openNoteM = Nothing
                }
    void $ defaultMain app initialModel

listItemHeight :: Int
listItemHeight = 1

orElse :: (Applicative m) => Maybe a -> m a -> m a
orElse m n = maybe n pure m

app :: App
app =
    Brick.App
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

appDraw :: Model -> [Widget]
appDraw Model{notes, openNoteM} = [mainWidget <=> keysHelpLine]
  where
    mainWidget = hBox $ agenda : toList openNoteWidget

    agenda =
        borderWithLabel
            (str "Agenda")
            (renderList renderListItem True notes & withVScrollBars OnRight)

    openNoteWidget = do
        Entity{entityVal = note@Note{note_text}} <- openNoteM
        let title = noteTitle note
            text = fromRgaM note_text
        pure $
            borderWithLabel (str title) $
                viewport OpenNoteViewport Both $
                    str text

    keysHelpLine =
        withAttr highlightAttr (str "Esc")
            <+> str " "
            <+> withAttr highlightAttr (str "q")
            <+> str " exit  "
            <+> withAttr highlightAttr (str "Enter")
            <+> str " open"

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

appHandleEvent :: BrickEvent -> EventM ()
appHandleEvent = \case
    Brick.VtyEvent e -> appHandleVtyEvent e
    _ -> pure ()

appHandleVtyEvent :: Event -> EventM ()
appHandleVtyEvent = \case
    EvKey KEsc [] -> halt
    EvKey (KChar 'q') [] -> halt
    EvKey KEnter [] -> do
        -- open selected note
        selectedNoteM <- preuse $ #notes . listSelectedElementL
        #openNoteM .= selectedNoteM
    e -> zoom #notes $ handleListEvent e

renderListItem :: Bool -> EntityDoc Note -> Widget
renderListItem _isSelected Entity{entityVal} = str $ noteTitle entityVal

noteTitle :: Note -> String
noteTitle Note{note_text} =
    case filter (not . null) $ lines $ fromRgaM note_text of
        [] -> "..."
        [singleLine] -> singleLine
        firstLine : _ -> firstLine <> "..."
