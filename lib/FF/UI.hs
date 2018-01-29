{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.UI where

import           Data.List (genericLength)
import qualified Data.Text as Text
import           Text.PrettyPrint.Mainland (Doc, commasep, hang, indent, sep,
                                            stack, star, strictText, (<+/>),
                                            (</>), (<>), (<|>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (ModeMap (..), NoteView (..), Sample (..))

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
samplesInSections limit ModeMap{..} = stack
    [ sample labelOverdue  "ff search --overdue"   overdue
    , sample labelEndToday "ff search --today"     endToday
    , sample labelEndSoon  "ff search --soon"      endSoon
    , sample labelActual   "ff search --actual"    actual
    , sample labelStarting "ff search --starting"  starting
    , "To see more tasks, run:" .= ("ff --limit=" <> show (max 0 limit + 10))
    ]
  where
    labelOverdue  = "Overdue:"
    labelEndToday = "Due today:"
    labelEndSoon  = "Due soon:"
    labelActual   = "Actual:"
    labelStarting = "Starting soon:"

sample :: String -> String -> Sample -> Doc
sample _     _           Sample{total = 0}    = mempty
sample label cmdToSeeAll Sample{total, notes} =
    withHeader label . stack $
        map ((star <>) . indent 1 . noteView) notes
        ++ [toSeeAllLabel .= Pretty.text cmdToSeeAll | count /= total]
  where
    count = genericLength notes
    toSeeAllLabel = "To see all " <> show total <> " task(s), run:"

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
