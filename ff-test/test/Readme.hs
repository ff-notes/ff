{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}

module Readme (readmeTest) where

import           CMark (Node (..), NodeType (..), commonmarkToNode, optSafe)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Hedgehog (Property, evalIO, property, (===))
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

import           FF.Options (showHelp)

readmeTest :: TestTree
readmeTest = testProperty "readme" prop_readme

prop_readme :: Property
prop_readme = property $ do
    readme <- evalIO $ Text.readFile "../README.md"
    let readme' = fromMaybe noHelpSection $ parseMD readme
    ffhelp === readme'

ffhelp :: Text
ffhelp = Text.unlines [pref, Text.pack $ showHelp Nothing]

parseMD :: Text -> Maybe Text
parseMD s =
    let Node _ _ ns = commonmarkToNode [optSafe] s
        code = [block | Node _ (CODE_BLOCK _ block) _ <- ns]
        help = filter ((==) pref . Text.take (Text.length pref)) code
    in  if null help then Nothing else Just (head help)

pref :: Text
pref = "$ ff --help"

noHelpSection :: Text
noHelpSection = "There is no help section in the README.md"

