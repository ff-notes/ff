{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.UI where

import           Data.List (genericLength)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           Numeric.Natural (Natural)
import           Text.PrettyPrint.Mainland (Doc, brackets, commasep, hang,
                                            indent, sep, stack, star,
                                            strictText, (<+/>), (</>), (<>),
                                            (<|>))
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

instance Pretty Day where
    ppr = pshow

agenda :: Int -> Agenda -> Doc
agenda limit Agenda{ending, starting} = stack
    [ sample labelEnding   "ff search --started --ending" ending
    , sample labelStarting "ff search --starting"         starting
    , "to see more tasks, run:" .= ("ff --limit=" <> show (max 0 limit + 10))
    ]
  where
    labelEnding   n = show n <> " started task(s) ending soon:"
    labelStarting n = show n <> " task(s) starting soon:"

sample :: Template Natural -> Text -> Sample -> Doc
sample labelTemplate cmdToSeeAll Sample{notes, total} = stack $
    withHeader (labelTemplate count) (stack $ map ppr notes)
    : [toSeeAllLabel .= strictText cmdToSeeAll | count /= total]
  where
    count = genericLength notes
    toSeeAllLabel = "to see all " <> show total <> " task(s), run:"

instance Pretty NoteView where
    ppr NoteView{..} =
        hang indentation
            (sep $
                star
                : map strictText (Text.words text)
                ++ [brackets $ pshow nid])
        </> indent indentation (fieldsSep fields)
      where
        fields = "start" .= start : ["end" .= e | Just e <- pure end]
        fieldsSep docs = commasep docs <|> stack docs
