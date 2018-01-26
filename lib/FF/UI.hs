{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FF.UI where

import           Data.List (genericLength)
import qualified Data.Text as Text
import           Text.PrettyPrint.Mainland (Doc, commasep, hang, indent, sep,
                                            stack, strictText, (<+/>), (</>),
                                            (<>), (<|>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (Agenda (..), NoteView (..), Sample (..))

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 4

pshow :: Show a => a -> Doc
pshow = Pretty.text . show

agenda :: Int -> Agenda -> Doc
agenda limit Agenda{overdue, endingToday, endingSoon, starting} = stack
    [ sample labelOverdue     "ff search --overdue"   overdue
    , sample labelEndingToday "ff search --today"     endingToday
    , sample labelEndingSoon  "ff search --soon"      endingSoon
    , sample labelStarting    "ff search --starting"  starting
    , "to see more tasks, run:" .= ("ff --limit=" <> show (max 0 limit + 10))
    ]
  where
    labelOverdue     = "overdue task(s):"
    labelEndingToday = "task(s) for today:"
    labelEndingSoon  = "task(s) ending soon:"
    labelStarting    = "task(s) starting soon:"

sample :: String -> String -> Sample -> Doc
sample _     _           Sample{total = 0}    = mempty
sample label cmdToSeeAll Sample{total, notes} =
    withHeader label . stack $
        map noteView notes
        ++ [toSeeAllLabel .= Pretty.text cmdToSeeAll | count /= total]
  where
    count = genericLength notes
    toSeeAllLabel = "to see all " <> show total <> " task(s), run:"

noteView :: NoteView -> Doc
noteView NoteView{nid, text, start, end} =
    hang indentation noteText </> indent indentation (fieldsSep fields)
  where
    noteText = stack . map (sep . map strictText . Text.words) $ Text.lines text
    fields
        =   "id" .= pshow nid
        :   "start" .= pshow start
        : [ "end" .= pshow e | Just e <- pure end ]
    fieldsSep docs = commasep docs <|> stack docs
