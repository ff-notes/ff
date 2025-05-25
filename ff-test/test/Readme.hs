{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Readme (
    readmeTest,
)
where

import CMark (
    Node (Node),
    NodeType (CODE_BLOCK),
    commonmarkToNode,
    nodeToCommonmark,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, isPrefixOf)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)

import FF.Options (showHelp)

import FF.Test.Common (diffCmd)

readmeTest :: TestTree
readmeTest =
    goldenVsStringDiff
        "readme"
        diffCmd
        "../README.md"
        ( do
            bytes <- BS.readFile "../README.md"
            let text = decodeUtf8 bytes
            let node = commonmarkToNode [] text
            let node' = replaceHelp node
            let text' = nodeToCommonmark [] Nothing node'
            pure $ BSL.fromStrict $ encodeUtf8 text'
        )

progHelp :: Text
progHelp = Text.unlines [helpPrefix, Text.pack showHelp]

replaceHelp :: Node -> Node
replaceHelp (Node pos typ children) = Node pos typ (replaceInChildren children)
  where
    replaceInChildren nodes =
        [ case node of
            Node pos' (CODE_BLOCK info text) children'
                | helpPrefix `isPrefixOf` text ->
                    Node pos' (CODE_BLOCK info progHelp) children'
            _ -> node
        | node <- nodes
        ]

helpPrefix :: Text
helpPrefix = "$ ff --help"
