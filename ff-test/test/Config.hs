{-# LANGUAGE OverloadedStrings #-}

module Config (configTests) where

import           Control.Monad (void)
import           FF.CLI (runCmdConfig)
import           FF.Config (Config (..), defaultConfigUI, emptyConfig,
                            loadConfig, printConfig, saveConfig)
import           Hedgehog (Group (..), Property, PropertyT, checkSequential,
                           evalIO, property, (===))
import           System.Environment (setEnv)
import           System.IO (stderr)
import           System.IO.Silently (hCapture_)
import           System.IO.Temp (withSystemTempDirectory)

configTests :: IO ()
configTests = withSystemTempDirectory "tempHome" $ \tempHome -> do
    setEnv "HOME" tempHome
    void $ checkSequential testGroup

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

testGroup :: Group
testGroup = Group "config tests" [
    ("LoadConfig with an absent config file:", prop_loadNoConfig),
    ("LoadConfig with defined dataDir:", prop_loadJustConfig),
    ("LoadConfig with Nothing in dataDir field:", prop_loadNothingConfig),
    ("RunCmdConf with defined dataDir:", prop_runCmdConfigJust),
    ("RunCmdConf with Nothing in dataDir field:", prop_runCmdConfigNothing)
    ]
