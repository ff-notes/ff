{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (defaultMain, testGroup)

import Config (configTests)
import Database (databaseTests)
import FF.Test.Options (optionsTest)
import Readme (readmeTest)
import Regression (mkRegressionTest)

main :: IO ()
main = do
    regressionTest <- mkRegressionTest
    withSystemTempDirectory "ff-test" $ \tmp ->
        defaultMain $
            testGroup
                ""
                [ configTests
                , databaseTests
                , optionsTest
                , readmeTest
                , regressionTest tmp
                ]
