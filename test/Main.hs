{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           Data.Time (fromGregorian)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           FF (getAgenda)
import           FF.Storage (DocId (DocId), runStorage)
import           FF.Types (NoteView (..))

main :: IO ()
main =
    defaultMain $ testCase "getAgenda" $ do
        assertEqual "not exist" (Map.fromList [])
            =<< runStorage "test/data/not exist" undefined getAgenda
        assertEqual "smoke"
            (Map.singleton
                (DocId "1")
                NoteView
                    { text = "hello"
                    , start = fromGregorian 22 11 24
                    , end = Just $ fromGregorian 17 06 19
                    })
            =<< runStorage "test/data/smoke.in" undefined getAgenda
