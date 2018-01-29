{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import           Data.Time (fromGregorian)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           FF (getSamples)
import           FF.Storage (DocId (DocId), runStorage)
import           FF.Types (NoteView (..), Sample (..), TaskMode (..))

main :: IO ()
main =
    defaultMain $ testCase "getSamples" $ do
        assertEqual "not exist" mempty
            =<< runStorage
                "test/data/not exist"
                undefined
                (getSamples agendaLimit)
        assertEqual "smoke"
            (Map.singleton
                Overdue
                Sample
                    { notes =
                        [NoteView
                            { nid = DocId "1"
                            , text = "hello"
                            , start = fromGregorian 22 11 24
                            , end = Just $ fromGregorian 17 06 19
                            }]
                    , total = 1
                    })
            =<< runStorage
                "test/data/smoke.in"
                undefined
                (getSamples agendaLimit)
  where
    agendaLimit = 10
