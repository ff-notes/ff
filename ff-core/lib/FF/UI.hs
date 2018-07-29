{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI where

import           Data.Char (isSpace)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           Text.PrettyPrint.Mainland (Doc, hang, indent, sep, space,
                                            stack, star, strictText, string,
                                            (<+/>), (<+>), (</>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (ModeMap, NoteId, NoteView (..), Sample (..),
                           TaskMode (..), Tracked (..), omitted)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 2

pshow :: Show a => a -> Doc
pshow = string . show

prettySamplesBySections :: Bool -> ModeMap Sample -> Doc
prettySamplesBySections brief samples = stack' brief $
    [prettySample brief mode sample | (mode, sample) <- Map.assocs samples] ++
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample :: Bool -> TaskMode -> Sample -> Doc
prettySample brief mode = \case
    Sample{total = 0} -> mempty
    Sample{total, notes} ->
        withHeader (labels mode) . stack' brief $
            map ((star <>) . indent 1 . noteView) notes
            ++  [ toSeeAllLabel .= Pretty.text (cmdToSeeAll mode)
                | count /= total
                ]
      where
        toSeeAllLabel = "To see all " <> show total <> " task(s), run:"
        count         = genericLength notes
        noteView = if brief then noteViewBrief else noteViewFull
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

noteViewBrief :: NoteView -> Doc
noteViewBrief NoteView{..} = title text <+/> meta
  where
    meta = foldMap (\i -> "| id" <+> pshow @NoteId i) nid
    title
        = stack
        . map (sep . map strictText . Text.split isSpace)
        . take 1
        . Text.lines

noteViewFull :: NoteView -> Doc
noteViewFull NoteView{..} = sparsedStack [wrapLines text, sep meta]
  where
    meta
        = concat
            [ ["| id"    <+> pshow @NoteId i | Just i <- [nid]]
            , ["| start" <+> pshow @Day start]
            , ["| end"   <+> pshow @Day e | Just e <- [end]]
            ]
        ++ concat
            [   [ "| tracking" <+> strictText trackedSource
                , "| url"      <+> strictText trackedUrl
                ]
            | Just Tracked{..} <- [tracked]
            ]

wrapLines :: Text -> Doc
wrapLines =
    stack . map (sep . map strictText . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc] -> Doc
sparsedStack = stack . intersperse space

stack' :: Bool -> [Doc] -> Doc
stack' brief = if brief then stack else sparsedStack
