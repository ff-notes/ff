{-# LANGUAGE OverloadedStrings #-}

module Readme (readmeTest) where

import           CMark (Node (Node), NodeType (CODE_BLOCK), commonmarkToNode,
                        optSafe)
import           Control.Error.Util (note)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Hedgehog (Property, evalEither, evalIO, property, (===))
import           Safe (headMay)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

import           FF.Options (showHelp)

readmeTest :: TestTree
readmeTest = testProperty "readme" prop_readme

prop_readme :: Property
prop_readme = property $ do
    readme <- evalIO $ Text.readFile "../README.md"
    readme' <- evalEither $ note noHelpSection $ parseFFHelp readme
    ffhelp === readme'

ffhelp :: Text
ffhelp = Text.unlines [pref, Text.pack showHelp]

parseFFHelp :: Text -> Maybe Text
parseFFHelp s =
    let Node _ _ ns = commonmarkToNode [optSafe] s
        code = [block | Node _ (CODE_BLOCK _ block) _ <- ns]
        help = filter ((==) pref . Text.take (Text.length pref)) code
    in  headMay help

pref :: Text
pref = "$ ff --help"

noHelpSection :: Text
noHelpSection = "There is no help section in the README.md"

