{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (configTests) where

import           Hedgehog (Property, PropertyT, evalIO, property, (===))
import           System.IO (stderr)
import           System.IO.Silently (hCapture_)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           FF.CLI (runCmdConfig)
import           FF.Config (Config (..), defaultConfigUI, emptyConfig,
                            loadConfig, printConfig, saveConfig)

configTests :: TestTree
configTests = $(testGroupGenerator)

captureString :: IO a -> PropertyT IO String
captureString = evalIO . hCapture_ [stderr]

checkRunCmd :: Config -> Property
checkRunCmd config = property $ do
    evalIO $ saveConfig config
    runcmdconf <- captureString $ runCmdConfig config Nothing
    printsetconf <- captureString $ printConfig config
    runcmdconf === printsetconf

checkLoad :: Config -> Property
checkLoad config = property $ do
    evalIO $ saveConfig config
    loadedconf <- evalIO loadConfig
    config === loadedconf

prop_loadNoConfig = property $ do
    config <- evalIO loadConfig
    emptyConfig === config

prop_loadJustConfig      = checkLoad   $ Config (Just "path") defaultConfigUI
prop_loadNothingConfig   = checkLoad   $ Config Nothing       defaultConfigUI
prop_runCmdConfigJust    = checkRunCmd $ Config (Just "path") defaultConfigUI
prop_runCmdConfigNothing = checkRunCmd $ Config Nothing       defaultConfigUI
