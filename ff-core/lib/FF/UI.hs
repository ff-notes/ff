{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FF.UI where

import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.PrettyPrint.Mainland (Doc, hang, indent, sep, stack, star,
                                            strictText, (<+/>), (</>), (<>),
                                            (<|>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (Limit, ModeMap, NoteView (..), Sample (..),
                           TaskMode (..), omitted)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 4

pshow :: Show a => a -> Doc
pshow = Pretty.text . show

prettySamplesBySections :: Limit -> ModeMap Sample -> Doc
prettySamplesBySections limit samples = stack $
    [prettySample mode sample | (mode, sample) <- Map.assocs samples] ++
    [ (show numOmitted <> " task(s) omitted. To see more tasks, run:")
      .= ("ff --limit=" <> show (max 0 limit + 10))
    | numOmitted > 0
    ]
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
noteView NoteView { nid, text, start, end } = noteText </> fieldsSep fields
  where
    noteText =
        stack . map (sep . map strictText . Text.words) $ Text.lines text
    fields =
        "id"
            .= pshow nid
            :  "start"
            .= pshow start
            :  [ "end" .= pshow e | Just e <- pure end ]
    fieldsSep docs = sep (intersperse "|" docs) <|> stack docs
