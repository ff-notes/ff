{-# LANGUAGE OverloadedStrings #-}

module Readme
  ( readmeTest,
  )
where

import CMark (Node (Node), NodeType (CODE_BLOCK), commonmarkToNode, optSafe)
import Control.Error (note)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import FF.Options (showHelp)
import Hedgehog ((===), PropertyT, evalEither, evalIO, property, withTests)
import Safe (headMay)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

readmeTest :: TestTree
readmeTest = testCase "readme" $ do
  readmeBS <- evalIO $ BS.readFile "../README.md"
  readmeText <- evalEither $ decodeUtf8' readmeBS
  readme <- evalEither $ parseFFHelp readmeText
  ffhelp === Text.lines readme

ffhelp :: [Text]
ffhelp = pref : map Text.stripEnd (Text.lines $ Text.pack showHelp)

parseFFHelp :: Text -> Either String Text
parseFFHelp s = note "no help found" $ headMay help
  where
    Node _ _ ns = commonmarkToNode [optSafe] s
    code = [block | Node _ (CODE_BLOCK _ block) _ <- ns]
    help = filter ((==) pref . Text.take (Text.length pref)) code

pref :: Text
pref = "$ ff --help"

testCase :: String -> PropertyT IO () -> TestTree
testCase name action = testProperty name $ withTests 1 $ property action
