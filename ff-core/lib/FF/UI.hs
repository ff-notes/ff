{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FF.UI where

import qualified Data.FullMap as FullMap
import           Data.List (genericLength)
import qualified Data.Text as Text
import           Text.PrettyPrint.Mainland (Doc, commasep, hang, indent, sep,
                                            stack, star, strictText, (<+/>),
                                            (</>), (<>), (<|>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (ModeMap, NoteView (..), Sample (..), TaskMode (..),
                           omitted)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 4

pshow :: Show a => a -> Doc
pshow = Pretty.text . show

samplesInSections :: Int -> ModeMap Sample -> Doc
samplesInSections limit samples = stack $
    [ sample samples Overdue
    , sample samples EndToday
    , sample samples EndSoon
    , sample samples Actual
    , sample samples Starting
    ] ++
    [ (show numOmitted <> " task(s) omitted. To see more tasks, run:")
      .= ("ff --limit=" <> show (max 0 limit + 10))
    | numOmitted > 0
    ]
  where
    numOmitted = sum $ fmap omitted samples

sample :: ModeMap Sample -> TaskMode -> Doc
sample samples mode = sample' $ FullMap.lookup mode samples
  where
    sample' Sample{ total = 0 } = mempty
    sample' Sample{ total, notes } =
        withHeader (labels mode) . stack $
            map ((star <>) . indent 1 . noteView) notes
            ++  [ toSeeAllLabel .= Pretty.text (cmdToSeeAll mode)
                | count /= total
                ]
      where
        toSeeAllLabel = "To see all " <> show total <> " task(s), run:"
        count         = genericLength notes
    labels = \case
        Overdue  -> "Overdue:"
        EndToday -> "Due today:"
        EndSoon  -> "Due soon:"
        Actual   -> "Actual:"
        Starting -> "Starting soon:"
    cmdToSeeAll = \case
        Overdue  -> "ff search --overdue"
        EndToday -> "ff search --today"
        EndSoon  -> "ff search --soon"
        Actual   -> "ff search --actual"
        Starting -> "ff search --starting"

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
    fieldsSep docs = commasep docs <|> stack docs
