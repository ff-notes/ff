{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick (
    AttrMap,
    AttrName,
    Direction (Down, Up),
    HScrollBarOrientation (OnBottom),
    VScrollBarOrientation (OnRight),
    ViewportType (Both),
    attrMap,
    attrName,
    defaultMain,
    hBox,
    hScrollBy,
    halt,
    neverShowCursor,
    on,
    txt,
    vScrollBy,
    vScrollPage,
    vScrollToBeginning,
    vScrollToEnd,
    viewport,
    viewportScroll,
    withAttr,
    withBorderStyle,
    withHScrollBars,
    withVScrollBars,
    zoom,
    (<+>),
    (<=>),
 )
import Brick qualified
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (borderStyleFromChar, unicode)
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listSelectedAttr,
    listSelectedElementL,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void, when)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty (
    Event (EvKey),
    Key (
        KChar,
        KDown,
        KEnd,
        KEnter,
        KEsc,
        KHome,
        KLeft,
        KPageDown,
        KPageUp,
        KRight,
        KUp
    ),
    Modifier (MCtrl),
    black,
    defAttr,
    white,
 )
import Lens.Micro.Mtl (preuse, use, (.=))
import RON.Storage.FS (runStorage)
import RON.Storage.FS qualified as StorageFS

import FF (fromRgaM, getDataDir, loadAllNotes, noDataDirectoryMessage)
import FF.Config (loadConfig)
import FF.Types (
    Entity (Entity),
    EntityDoc,
    Note (Note),
    NoteStatus (TaskStatus),
    Status (Active),
 )
import FF.Types qualified

-- | Brick widget names
data WN
    = NoteList
    | OpenNoteViewport
    deriving (Eq, Ord, Show)

data Model = Model
    { visibleNotes :: List WN (EntityDoc Note)
    , openNoteM :: Maybe (EntityDoc Note)
    -- ^ currently opened note, if opened
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
    allNotes <- runStorage handle loadAllNotes
    let filteredNotes = filter (isNoteActive . (.entityVal)) allNotes
    let initialModel =
            Model
                { visibleNotes =
                    list NoteList (Vector.fromList filteredNotes) listItemHeight
                , openNoteM = Nothing
                }
    void $ defaultMain app initialModel

isNoteActive :: Note -> Bool
isNoteActive Note{note_status} = note_status == Just (TaskStatus Active)

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
        , (listSelectedFocusedAttr, black `on` white)
        , (highlightAttr, black `on` white)
        ]

withBorderIf :: Bool -> Widget -> Widget
withBorderIf cond =
    withBorderStyle if cond then unicode else borderStyleFromChar ' '

appDraw :: Model -> [Widget]
appDraw Model{visibleNotes, openNoteM} = [mainWidget <=> keysHelpLine]
  where
    focusedWidget =
        case openNoteM of
            Nothing -> NoteList
            Just _ -> OpenNoteViewport

    mainWidget = hBox $ noteList : toList openNoteWidget

    noteList =
        border
            ( renderList renderListItem True visibleNotes
                & withVScrollBars OnRight
            )
            & withBorderIf (focusedWidget == NoteList)

    openNoteWidget = do
        Entity{entityVal = Note{note_text}} <- openNoteM
        let noteText = Text.pack $ filter (/= '\r') $ fromRgaM note_text
        Just $
            border
                ( viewport
                    OpenNoteViewport
                    Both
                    (txt {- TODO txtWrap? -} noteText)
                    & withHScrollBars OnBottom
                    & withVScrollBars OnRight
                )
                & withBorderIf (focusedWidget == OpenNoteViewport)

    keysHelpLine =
        case focusedWidget of
            NoteList ->
                withAttr highlightAttr (txt "^q")
                    <+> txt " "
                    <+> withAttr highlightAttr (txt "Esc")
                    <+> txt " exit  "
                    <+> withAttr highlightAttr (txt "Enter")
                    <+> txt " open"
            OpenNoteViewport ->
                withAttr highlightAttr (txt "^q")
                    <+> txt " exit  "
                    <+> withAttr highlightAttr (txt "Esc")
                    <+> txt " close"

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

appHandleEvent :: BrickEvent -> EventM ()
appHandleEvent = \case
    Brick.VtyEvent e -> appHandleVtyEvent e
    _ -> pure ()

appHandleVtyEvent :: Event -> EventM ()
appHandleVtyEvent event = do
    openNoteM <- use #openNoteM
    let focusedWidget =
            case openNoteM of
                Nothing -> NoteList
                Just _ -> OpenNoteViewport
    case focusedWidget of
        NoteList ->
            case event of
                EvKey (KChar 'q') [MCtrl] -> halt
                EvKey KEsc [] -> halt
                EvKey KEnter [] -> do
                    -- open selected note
                    selectedNoteM <-
                        preuse $ #visibleNotes . listSelectedElementL
                    #openNoteM .= selectedNoteM
                e -> zoom #visibleNotes $ handleListEvent e
        OpenNoteViewport ->
            case event of
                EvKey (KChar 'q') [MCtrl] -> halt
                EvKey KEsc [] ->
                    -- close note view
                    #openNoteM .= Nothing
                e -> when (isJust openNoteM) $ handleViewportEvent e

handleViewportEvent :: Event -> EventM ()
handleViewportEvent = \case
    EvKey KUp [] -> vScrollBy vps (-1)
    EvKey KDown [] -> vScrollBy vps 1
    EvKey KLeft [] -> hScrollBy vps (-1)
    EvKey KRight [] -> hScrollBy vps 1
    EvKey KHome [] -> vScrollToBeginning vps
    EvKey KEnd [] -> vScrollToEnd vps
    EvKey KPageUp [] -> vScrollPage vps Up
    EvKey KPageDown [] -> vScrollPage vps Down
    _ -> pure ()
  where
    vps = viewportScroll OpenNoteViewport

renderListItem :: Bool -> EntityDoc Note -> Widget
renderListItem _isSelected Entity{entityVal} = txt $ noteTitle entityVal

noteTitle :: Note -> Text
noteTitle Note{note_text} =
    case textLines of
        [] -> "..."
        [singleLine] -> singleLine
        firstLine : _ -> firstLine <> "..."
  where
    textLines =
        filter (not . Text.null) $ Text.lines $ Text.pack $ fromRgaM note_text
