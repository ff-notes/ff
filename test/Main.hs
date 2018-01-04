{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           FF (Result (..), runAgenda)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)

runTest :: String -> IO Result -> Result -> Assertion
runTest name test expected = assertEqual name expected =<< test

main :: IO ()
main =
    defaultMain $ testCase "runAgenda" $
        runTest
            "smoke"
            (runAgenda "test/data/smoke.in")
            Result{notes = Map.fromList [("1", "hello")]}
