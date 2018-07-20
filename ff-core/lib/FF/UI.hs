{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI where

import           Data.Char (isSpace)
import           Data.List (genericLength)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           Text.PrettyPrint.Mainland (Doc, hang, indent, sep, stack, star,
                                            strictText, string, (<+/>), (</>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (ModeMap, NoteId, NoteView (..), Sample (..),
                           TaskMode (..), omitted)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 2

pshow :: Show a => a -> Doc
pshow = string . show

prettySamplesBySections :: ModeMap Sample -> Doc
prettySamplesBySections samples = stack $
    [prettySample mode sample | (mode, sample) <- Map.assocs samples] ++
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample :: TaskMode -> Sample -> Doc
prettySample mode = \case
    Sample{total = 0} -> mempty
    Sample{total, notes} ->
        withHeader (labels mode) . stack $
            map ((star <>) . indent 1 . noteView) notes
            ++  [ toSeeAllLabel .= Pretty.text (cmdToSeeAll mode)
                | count /= total
                ]
      where
        toSeeAllLabel = "To see all " <> show total <> " task(s), run:"
        count         = genericLength notes
  where
    labels = \case
        Overdue n -> case n of
            1 -> "1 day overdue:"
            _ -> show n <> " days overdue:"
        EndToday -> "Due today:"
        EndSoon n -> case n of
            1 -> "Due tomorrow:"
            _ -> "Due in " <> show n <> " days:"
        Actual -> "Actual:"
        Starting n -> case n of
            1 -> "Starting tomorrow:"
            _ -> "Starting in " <> show n <> " days:"
    cmdToSeeAll = \case
        Overdue _  -> "ff search --overdue"
        EndToday   -> "ff search --today"
        EndSoon _  -> "ff search --soon"
        Actual     -> "ff search --actual"
        Starting _ -> "ff search --starting"

noteView :: NoteView -> Doc
noteView NoteView{nid, text, start, end} = wrapLines text </> sep fields
  where
    fields = concat
        [ ["| id "    <> pshow @NoteId i | Just i <- [nid]]
        , ["| start " <> pshow @Day start]
        , ["| end "   <> pshow @Day e | Just e <- [end]]
        ]

wrapLines :: Text -> Doc
wrapLines =
    stack . map (sep . map strictText . Text.split isSpace) . Text.splitOn "\n"
