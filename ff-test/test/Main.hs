{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import           Test.Tasty (defaultMain, testGroup)
import           System.IO.Temp (withSystemTempDirectory)

import           Config (configTests)
import           Database (databaseTests)
import           Readme (readmeTest)
import           Regression (mkRegressionTest)

main :: IO ()
main = do
    regressionTest <- mkRegressionTest
    withSystemTempDirectory "ff-test" $ \tmp ->
        defaultMain $
            testGroup ""
                [configTests, databaseTests, readmeTest, regressionTest tmp]
