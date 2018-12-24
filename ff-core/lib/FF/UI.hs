{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI (
    prettyContact,
    prettyContactSample,
    prettyNote,
    prettyNoteList,
    prettyTaskSections,
    prettyTasksWikisContacts,
    prettyWikiSample,
    sampleLabel,
    withHeader,
) where

import           Data.Char (isSpace)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc (Doc, fillSep, hang, indent, pretty,
                                            sep, space, viaShow, vsep, (<+>))
import           Data.Time (Day)
import           RON.Storage (DocId (DocId))

import           FF.Types (Contact (..), ContactSample, Entity (..), ModeMap,
                           Note (..), NoteSample, NoteStatus (Wiki),
                           Sample (..), TaskMode (..), Track (..), omitted)

withHeader :: Text -> Doc ann -> Doc ann
withHeader header value = hang indentation $ vsep [pretty header, value]

indentation :: Int
indentation = 2

prettyDocId :: DocId a -> Doc ann
prettyDocId (DocId name) = pretty name

prettyTasksWikisContacts
    :: Bool                 -- ^ is output brief
    -> ModeMap NoteSample   -- ^ tasks
    -> NoteSample           -- ^ wikis
    -> ContactSample        -- ^ contacts
    -> Bool                 -- ^ does search involve tasks
    -> Bool                 -- ^ does search involve wikis
    -> Bool                 -- ^ does search involve contacts
    -> Doc ann
prettyTasksWikisContacts
        isBrief tasks wiki contacts involveTasks involveWikis involveContacts =
    case (involveTasks, involveWikis, involveContacts) of
        (True,  False, False) -> ts
        (False, True,  False) -> ws
        (False, False, True ) -> cs
        (True,  True,  False) -> vsep [ts, ws]
        (False, True,  True ) -> vsep [ws, cs]
        (True,  False, True ) -> vsep [ts, cs]
        (_,     _,     _    ) -> vsep [ts, ws, cs]
  where
    ts = prettyTaskSections isBrief tasks
    ws = prettyWikiSample        isBrief wiki
    cs = prettyContactSample     isBrief contacts

prettyContactSample :: Bool -> ContactSample -> Doc ann
prettyContactSample isBrief samples = stack isBrief $
    prettyContactSample' samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples
    prettyContactSample' = \case
        Sample{sample_total = 0} -> "No contacts to show"
        Sample{sample_items} ->
            withHeader "Contacts:" . stack isBrief $
            map ((star <>) . indent 1 . prettyContact isBrief) sample_items

prettyWikiSample :: Bool -> NoteSample -> Doc ann
prettyWikiSample isBrief samples = stack isBrief $
    prettyWikiSample' samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples
    prettyWikiSample' = \case
        Sample{sample_total = 0} -> "No wikis to show"
        Sample{sample_items} ->
            withHeader "Wiki:" .
            stack isBrief $
            map ((star <>) . indent 1 . prettyNote isBrief) sample_items

prettyNoteList :: Bool -> [Entity Note] -> Doc ann
prettyNoteList isBrief =
    stack isBrief . map ((star <>) . indent 1 . prettyNote isBrief)

-- | For both tasks and wikis
prettyNote
    :: Bool  -- ^ is brief
    -> Entity Note
    -> Doc ann
prettyNote isBrief (Entity entityId Note{..}) = case isBrief of
    True -> fillSep [title note_text, meta] where
        meta = "| id" <+> prettyDocId entityId
    False -> sparsedStack [wrapLines $ Text.pack note_text, sep meta] where
        meta
            = mconcat
                [   [ "| id" <+> prettyDocId entityId
                    | entityId /= DocId ""
                    ]
                ,   [ "| start" <+> viaShow @Day note_start
                    | note_status /= Wiki
                    ]
                ,   [ "| end" <+> viaShow @Day end
                    | note_status /= Wiki
                    , Just end <- [note_end]
                    ]
                ]
            ++  [ "| tracking" <+> pretty track_url
                | Just Track{..} <- [note_track]
                ]

title :: String -> Doc ann
title
    = mconcat
    . map (fillSep . map pretty . Text.split isSpace)
    . take 1
    . Text.lines
    . Text.pack

prettyTaskSections :: Bool -> ModeMap (Sample (Entity Note)) -> Doc ann
prettyTaskSections isBrief samples = stack isBrief
    $   [ prettyTaskSample isBrief mode sample
        | (mode, sample) <- Map.assocs samples
        ]
    ++  [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettyTaskSample :: Bool -> TaskMode -> Sample (Entity Note) -> Doc ann
prettyTaskSample isBrief mode = \case
    Sample{sample_total = 0} -> "No notes to show"
    Sample{sample_total, sample_items} ->
        withHeader (sampleLabel mode) . stack isBrief $
            map ((star <>) . indent 1 . prettyNote isBrief) sample_items
            ++  [ hang indentation $
                    fillSep [pretty toSeeAllLabel, cmdToSeeAll mode]
                | count /= sample_total
                ]
      where
        toSeeAllLabel =
            "To see all " <> Text.pack (show sample_total) <> " task(s), run:"
        count         = genericLength sample_items
  where
    cmdToSeeAll = \case
        Overdue _  -> "ff search --overdue"
        EndToday   -> "ff search --today"
        EndSoon _  -> "ff search --soon"
        Actual     -> "ff search --actual"
        Starting _ -> "ff search --starting"

sampleLabel :: TaskMode -> Text
sampleLabel = \case
    Overdue n -> case n of
        1 -> "1 day overdue:"
        _ -> Text.pack (show n) <> " days overdue:"
    EndToday -> "Due today:"
    EndSoon n -> case n of
        1 -> "Due tomorrow:"
        _ -> "Due in " <> Text.pack (show n) <> " days:"
    Actual -> "Actual:"
    Starting n -> case n of
        1 -> "Starting tomorrow:"
        _ -> "Starting in " <> Text.pack (show n) <> " days:"

prettyContact :: Bool -> Entity Contact -> Doc ann
prettyContact _isBrief (Entity entityId Contact{..}) =
    sep [pretty contact_name, meta]
  where
    meta = "| id" <+> prettyDocId entityId

wrapLines :: Text -> Doc ann
wrapLines =
    vsep . map (fillSep . map pretty . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc ann] -> Doc ann
sparsedStack = vsep . intersperse space

stack
    :: Bool  -- ^ is brief
    -> [Doc ann]
    -> Doc ann
stack = \case
    True  -> vsep
    False -> sparsedStack

star :: Doc ann
star = "*"
