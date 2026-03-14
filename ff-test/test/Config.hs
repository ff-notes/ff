{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (configTests) where

import Hedgehog (Property, evalIO, forAll, property, withTests, (===))
import System.Directory (withCurrentDirectory)
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import FF.Config (emptyConfig, loadConfig, saveConfig)

import Gen qualified

configTests :: TestTree
configTests = $testGroupGenerator

prop_loadNoConfig :: Property
prop_loadNoConfig =
    withTests 1 $
        property do
            config <-
                evalIO . withCurrentDirectory "/" $ withTempHome loadConfig
            emptyConfig === config

prop_loadConfig :: Property
prop_loadConfig =
    property do
        config <- forAll Gen.config
        loadedconf <-
            evalIO
                . withCurrentDirectory "/"
                $ withTempHome do
                    saveConfig config
                    loadConfig
        config === loadedconf

withTempHome :: IO a -> IO a
withTempHome action =
    withSystemTempDirectory "ff-test.home" \tempHome -> do
        setEnv "HOME" tempHome
        action
