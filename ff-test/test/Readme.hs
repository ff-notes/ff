{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Readme (readmeTest) where

import           CMark (Node (..), NodeType (..), commonmarkToNode, optSafe)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Hedgehog (Property, evalIO, property, (===))
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           FF.Options (showHelp)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


readmeTest :: TestTree
readmeTest = $(testGroupGenerator)

prop_readme :: Property
prop_readme = property $ do
    readme <- evalIO $ TIO.readFile "../README.md"
    let readme' = fromMaybe noHelpSection $ parseMD readme
    ffhelp === readme'

ffhelp :: Text
ffhelp = T.concat [pref, "\n", T.pack $ showHelp Nothing, "\n"]

parseMD :: Text -> Maybe Text
parseMD s =
    let Node _ _ ns = commonmarkToNode [optSafe] s
        code = [block | Node _ (CODE_BLOCK _ block) _ <- ns]
        help = filter ((==) pref . T.take (T.length pref)) code
        help' = if null help then Nothing else Just (head help)
    in  help'

pref :: Text
pref = "$ ff --help"

noHelpSection :: Text
noHelpSection = "There is no a help section in the README.md"

