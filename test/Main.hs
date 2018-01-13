{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           FF (getAgenda)
import           FF.Storage (DocId (DocId), runStorage)

main :: IO ()
main =
    defaultMain $ testCase "getAgenda" $ do
        assertEqual "not exist" (Map.fromList [])
            =<< runStorage "test/data/not exist" undefined getAgenda
        assertEqual "smoke" (Map.fromList [(DocId "1", "hello")])
            =<< runStorage "test/data/smoke.in" undefined getAgenda
