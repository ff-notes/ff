{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FF.UI where

import           Control.Error ((?:))
import           Data.List (genericLength)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.PrettyPrint.Mainland (Doc, commasep, hang, indent, sep,
                                            stack, star, strictText, (<+/>),
                                            (</>), (<>), (<|>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (NoteView (..), Sample (..), Samples, TaskMode (..),
                           emptySample)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 4

pshow :: Show a => a -> Doc
pshow = Pretty.text . show

samplesInSections :: Int -> Samples -> Doc
samplesInSections limit samples = stack
    [ sample labelOverdue  "ff search --overdue"   overdue
    , sample labelEndToday "ff search --today"     endToday
    , sample labelEndSoon  "ff search --soon"      endSoon
    , sample labelStarting "ff search --starting"  starting
    , "to see more tasks, run:" .= ("ff --limit=" <> show (max 0 limit + 10))
    ]
  where
    overdue  = Map.lookup Overdue  samples ?: emptySample
    endToday = Map.lookup EndToday samples ?: emptySample
    endSoon  = Map.lookup EndSoon  samples ?: emptySample
    starting = Map.lookup Starting samples ?: emptySample
    labelOverdue  = "Overdue:"
    labelEndToday = "Due today:"
    labelEndSoon  = "Due soon:"
    labelStarting = "Starting soon:"

sample :: String -> String -> Sample -> Doc
sample _     _           Sample{total = 0}    = mempty
sample label cmdToSeeAll Sample{total, notes} =
    withHeader label . stack $
        map ((star <>) . indent 1 . noteView) notes
        ++ [toSeeAllLabel .= Pretty.text cmdToSeeAll | count /= total]
  where
    count = genericLength notes
    toSeeAllLabel = "to see all " <> show total <> " task(s), run:"

noteView :: NoteView -> Doc
noteView NoteView{nid, text, start, end} =
    noteText </> fieldsSep fields
  where
    noteText = stack . map (sep . map strictText . Text.words) $ Text.lines text
    fields
        =   "id" .= pshow nid
        :   "start" .= pshow start
        : [ "end" .= pshow e | Just e <- pure end ]
    fieldsSep docs = commasep docs <|> stack docs
