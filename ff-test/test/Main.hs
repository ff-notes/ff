{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import           Test.Tasty (defaultMain, testGroup)

import           Config (configTests)
import           Database (dataTests)
import           Readme (readmeTest)

main :: IO ()
main = defaultMain $ testGroup "" [configTests, dataTests, readmeTest]
