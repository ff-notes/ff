{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           FF (DocId (DocId), cmdAgenda)

main :: IO ()
main =
    defaultMain $ testCase "cmdAgenda" $
        assertEqual "smoke" (Map.fromList [(DocId "1", "hello")])
        =<< cmdAgenda "test/data/smoke.in"
