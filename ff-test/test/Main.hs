{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import           System.Environment (setEnv)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty (defaultMain, testGroup)

import           Config (configTests)
import           Database (dataTests)

main :: IO ()
main =
    withSystemTempDirectory "ff-test.home" $ \tempHome -> do
        setEnv "HOME" tempHome
        defaultMain $ testGroup "" [configTests, dataTests]
