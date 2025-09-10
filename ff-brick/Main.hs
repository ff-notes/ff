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
    ViewportType (Vertical),
    attrMap,
    attrName,
    defaultMain,
    hBox,
    hScrollBy,
    halt,
    neverShowCursor,
    on,
    txt,
    txtWrap,
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
import Brick.Widgets.Border (border, hBorderWithLabel)
import Brick.Widgets.Border.Style (borderStyleFromChar, unicode)
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listSelectedAttr,
    listSelectedElement,
    listSelectedElementL,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
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

import FF (
    defaultNoteFilter,
    fromRgaM,
    getDataDir,
    getUtcToday,
    loadAllNotes,
    noDataDirectoryMessage,
    viewTaskSamples,
 )
import FF.Config (loadConfig)
import FF.Config qualified
import FF.Types (
    Entity (Entity),
    EntityView,
    ModeMap,
    Note (Note),
    NoteSample,
    Sample (Sample),
    TaskMode,
    View (NoteView),
 )
import FF.Types qualified
import FF.UI (sampleLabel)

-- | Widget names
data WN
    = NoteList
    | OpenNoteViewport
    deriving (Eq, Ord, Show)

data Model = Model
    { visibleNotes ::
        List
            WN
            ( Either
                TaskMode -- section title
                (EntityView Note) -- item
            )
    , isNoteOpen :: Bool
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
    today <- getUtcToday
    let limit = Nothing
    samples <-
        runStorage handle do
            allNotes <- loadAllNotes
            viewTaskSamples
                defaultNoteFilter
                cfg.ui
                limit
                today
                allNotes
    let initialModel =
            Model
                { visibleNotes =
                    list NoteList (notesToModel samples) listItemHeight
                , isNoteOpen = False
                }
    void $ defaultMain app initialModel

notesToModel :: ModeMap NoteSample -> Vector (Either TaskMode (EntityView Note))
notesToModel = Vector.fromList . foldMap notesToModel' . Map.assocs
  where
    notesToModel' (taskMode, Sample{items}) = Left taskMode : map Right items

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

withVisibleBorderIf :: Bool -> Widget -> Widget
withVisibleBorderIf cond =
    withBorderStyle if cond then unicode else borderStyleFromChar ' '

appDraw :: Model -> [Widget]
appDraw Model{visibleNotes, isNoteOpen} = [mainWidget <=> keysHelpLine]
  where
    mainWidget = noteList <+> openNoteWidget

    noteList =
        border
            ( renderList renderListItem True visibleNotes
                & withVScrollBars OnRight
            )
            & withVisibleBorderIf (not isNoteOpen)

    openNoteWidget =
        border
            ( viewport OpenNoteViewport Vertical (txtWrap openNoteContent)
                & withHScrollBars OnBottom
                & withVScrollBars OnRight
            )
            & withVisibleBorderIf isNoteOpen

    openNoteContent =
        case listSelectedElement visibleNotes of
            Just (_, Right (Entity _ NoteView{note})) ->
                Text.pack $ clean $ fromRgaM note.note_text
            _ -> ""

    keysHelpLine =
        hBox
            if isNoteOpen then
                [ withAttr highlightAttr (txt "^q")
                , txt " exit  "
                , withAttr highlightAttr (txt "Esc")
                , txt " close"
                ]
            else
                [ withAttr highlightAttr (txt "^q")
                , txt " "
                , withAttr highlightAttr (txt "Esc")
                , txt " exit"
                ]
                    ++ case listSelectedElement visibleNotes of
                        Just (_, Right Entity{}) ->
                            [ txt "  "
                            , withAttr highlightAttr (txt "Enter")
                            , txt " open"
                            ]
                        _ -> []

-- Clean string for Brick
clean :: String -> String
clean = filter (/= '\r')

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

appHandleEvent :: BrickEvent -> EventM ()
appHandleEvent = \case
    Brick.VtyEvent e -> appHandleVtyEvent e
    _ -> pure ()

appHandleVtyEvent :: Event -> EventM ()
appHandleVtyEvent event = do
    isNoteOpen <- use #isNoteOpen
    if isNoteOpen then case event of
        EvKey (KChar 'q') [MCtrl] -> halt
        EvKey KEsc [] ->
            -- close note view
            #isNoteOpen .= False
        e -> handleViewportEvent e
    else case event of
        EvKey (KChar 'q') [MCtrl] -> halt
        EvKey KEsc [] -> halt
        EvKey KEnter [] -> do
            -- open selected note
            selectedNoteM <-
                preuse $ #visibleNotes . listSelectedElementL
            #isNoteOpen
                .= case selectedNoteM of
                    Just (Right Entity{}) -> True
                    _ -> False
        e -> zoom #visibleNotes $ handleListEvent e

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

renderListItem :: Bool -> Either TaskMode (EntityView Note) -> Widget
renderListItem _isSelected = \case
    Left taskMode -> hBorderWithLabel $ txt $ sampleLabel taskMode
    Right (Entity _ NoteView{note}) -> txt $ noteTitle note

noteTitle :: Note -> Text
noteTitle Note{note_text} =
    case textLines of
        [] -> "..."
        [singleLine] -> singleLine
        firstLine : _ -> firstLine <> "..."
  where
    textLines =
        filter (not . Text.null) $ Text.lines $ Text.pack $ fromRgaM note_text
