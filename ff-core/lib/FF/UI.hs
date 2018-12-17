{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI (
    contactViewFull,
    noteViewFull,
    prettyContactSamplesOmitted,
    prettyNotes,
    prettyNotesWikiContacts,
    prettySamplesBySections,
    prettyWikiSamplesOmitted,
    sampleFmap,
    sampleLabel,
    withHeader,
) where

import           Data.Char (isSpace)
import           Data.Foldable (toList)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as TextL
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), fillSep, hang,
                                            indent, sep, space, viaShow, vsep,
                                            (<+>))
import           Data.Time (Day)
import           RON.Text.Serialize (serializeUuid)
import           RON.Types (UUID)

import           FF.Types (Contact (..), ContactSample, Entity, pattern Entity,
                           EntityF (..), ModeMap, Note (..), NoteSample,
                           Sample (..), TaskMode (..), Track (..), omitted)

withHeader :: Pretty ann => Text -> Doc ann -> Doc ann
withHeader header value = hang indentation $ vsep [pretty header, value]

indentation :: Int
indentation = 2

prettyUuid :: UUID -> Doc ann
prettyUuid = pretty . TextL.decodeUtf8 . serializeUuid

prettyNotesWikiContacts
    :: Pretty ann
    => Bool  -- ^ brief output
    -> ModeMap NoteSample
    -> NoteSample
    -> ContactSample
    -> Bool  -- ^ search among tasks
    -> Bool  -- ^ search among wiki notes
    -> Bool  -- ^ search among contacts
    -> Doc ann
prettyNotesWikiContacts brief notes wiki contacts amongN amongW amongC =
    case (amongN, amongW, amongC) of
        (True,  False, False) -> ns
        (False, True,  False) -> ws
        (False, False, True ) -> cs
        (True,  True,  False) -> vsep [ns, ws]
        (False, True,  True ) -> vsep [ws, cs]
        (True,  False, True ) -> vsep [ns, cs]
        (_,     _,     _    ) -> vsep [ns, ws, cs]
  where
    ns = prettySamplesBySections brief notes
    ws = prettyWikiSamplesOmitted brief wiki
    cs = prettyContactSamplesOmitted brief contacts

prettyContactSamplesOmitted :: Pretty ann => Bool -> ContactSample -> Doc ann
prettyContactSamplesOmitted brief samples = stack' brief $
    prettyContactSample True samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyContactSample :: Pretty ann => Bool -> ContactSample -> Doc ann
prettyContactSample brief = \case
    Sample{sample_total = 0} -> "No contacts to show."
    Sample{sample_items} ->
        withHeader "Contacts:" . stack' brief $
        map ((star <>) . indent 1 . contactViewFull) sample_items

prettyWikiSamplesOmitted :: Pretty ann => Bool -> NoteSample -> Doc ann
prettyWikiSamplesOmitted brief samples = stack' brief $
    prettyWikiSample brief samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyNotes :: Foldable f => Bool -> [EntityF f Note] -> Doc ann
prettyNotes brief = stack' brief . map ((star <>) . indent 1 . noteView brief)

prettyWikiSample :: Pretty ann => Bool -> NoteSample -> Doc ann
prettyWikiSample brief = \case
    Sample{sample_total = 0} -> "No wiki to show."
    Sample{sample_items} ->
        withHeader "Wiki notes:" .
        stack' brief $
        map ((star <>) . indent 1 . noteView brief) sample_items

noteView :: Foldable f => Bool -> EntityF f Note -> Doc ann
noteView brief = if brief then noteViewBrief else noteViewFull

prettySamplesBySections
    :: (Pretty ann, Foldable f)
    => Bool
    -> ModeMap (Sample (EntityF f Note))
    -> Doc ann
prettySamplesBySections brief samples = stack' brief
    $   [prettySample brief mode sample | (mode, sample) <- Map.assocs samples]
    ++  [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample
    :: (Pretty ann, Foldable f)
    => Bool
    -> TaskMode
    -> Sample (EntityF f Note)
    -> Doc ann
prettySample brief mode = \case
    Sample{sample_total = 0} -> "No notes to show."
    Sample{sample_total, sample_items} ->
        withHeader (sampleLabel mode) . stack' brief $
            map ((star <>) . indent 1 . noteView brief) sample_items
            ++  [ cmdToSeeAll mode
                | count /= sample_total
                ]
      where
        count = genericLength sample_items
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

noteViewBrief :: Foldable f => EntityF f Note -> Doc ann
noteViewBrief (EntityF fEntityId Note{..}) = fillSep [title, meta]
  where
    meta = foldMap (\i -> "| id" <+> prettyUuid i) fEntityId
    title
        = vsep
        . map (fillSep . map pretty . Text.split isSpace)
        . take 1
        . Text.lines
        $ Text.pack note_text

noteViewFull :: Foldable f => EntityF f Note -> Doc ann
noteViewFull (EntityF fEntityId Note{..}) =
    sparsedStack [wrapLines $ Text.pack note_text, sep meta]
  where
    meta
        = concat
            [ ["| id"    <+> prettyUuid eid | eid <- toList fEntityId]
            , ["| start" <+> viaShow @Day note_start]
            , ["| end"   <+> viaShow @Day e | Just e <- [note_end]]
            ]
        ++  [ "| tracking" <+> pretty track_url
            | Just Track{..} <- [note_track]
            ]

contactViewFull :: Entity Contact -> Doc ann
contactViewFull (Entity entityId Contact{..}) =
    sep [pretty contact_name, meta]
  where
    meta = "| id" <+> prettyUuid entityId

wrapLines :: Text -> Doc ann
wrapLines =
    vsep . map (fillSep . map pretty . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc ann] -> Doc ann
sparsedStack = vsep . intersperse space

stack' :: Bool -> [Doc ann] -> Doc ann
stack' brief
    | brief     = vsep
    | otherwise = sparsedStack

sampleFmap :: (a -> b) -> Sample a -> Sample b
sampleFmap f sample@Sample{sample_items} =
    sample{sample_items = map f sample_items}

star :: Doc ann
star = "*"
