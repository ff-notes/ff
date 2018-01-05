{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           FF (cmdAgenda)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main =
    defaultMain $ testCase "cmdAgenda" $
        assertEqual "smoke" (Map.fromList [("1", "hello")])
        =<< cmdAgenda "test/data/smoke.in"
