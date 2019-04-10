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

captureString :: IO () -> PropertyT IO String
captureString = evalIO . hCapture_ [stderr]

checkRunCmd :: Config -> PropertyT IO ()
checkRunCmd setConfig = do
    evalIO $ saveConfig setConfig
    runcmdconf <- captureString $ runCmdConfig setConfig Nothing
    printsetconf <- captureString $ printConfig setConfig
    runcmdconf === printsetconf

checkLoad :: Config -> PropertyT IO ()
checkLoad setConfig = do
    evalIO $ saveConfig setConfig
    loadedconf <- evalIO loadConfig
    setConfig === loadedconf

prop_loadNoConfig :: Property
prop_loadNoConfig = property $ do
    conf <- evalIO loadConfig
    emptyConfig === conf

prop_loadJustConfig :: Property
prop_loadJustConfig = property $ do
    let setConfig = Config (Just "pathNotes") defaultConfigUI
    checkLoad setConfig

prop_loadNothingConfig :: Property
prop_loadNothingConfig = property $ do
    let setConfig = Config Nothing defaultConfigUI
    checkLoad setConfig

prop_runCmdConfigJust :: Property
prop_runCmdConfigJust = property $ do
    let setConfig = Config (Just "pathNotes") defaultConfigUI
    checkRunCmd setConfig

prop_runCmdConfigNothing :: Property
prop_runCmdConfigNothing = property $ do
    let setConfig = Config Nothing defaultConfigUI
    checkRunCmd setConfig
