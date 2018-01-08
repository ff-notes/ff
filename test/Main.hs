{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           FF (cmdAgenda)
import           FF.Storage (DocId (DocId), runStorage)

main :: IO ()
main =
    defaultMain $ testCase "cmdAgenda" $ do
        assertEqual "not exist" (Map.fromList [])
            =<< runStorage "test/data/not exist" undefined cmdAgenda
        assertEqual "smoke" (Map.fromList [(DocId "1", "hello")])
            =<< runStorage "test/data/smoke.in" undefined cmdAgenda
