{-# LANGUAGE OverloadedStrings #-}

module Config (configTests) where

import           Control.Exception (bracket)
import           Control.Monad (void)
import           FF.CLI (runCmdConfig)
import           FF.Config (Config (..), defaultConfigUI, emptyConfig,
                            loadConfig, printConfig, saveConfig)
import           Hedgehog (Group (..), Property, PropertyT, checkSequential,
                           evalIO, property, (===))
import           System.Environment (getEnv, setEnv)
import           System.IO (stderr)
import           System.IO.Silently (hCapture_)
import           System.IO.Temp (withSystemTempDirectory)

configTests :: IO ()
configTests = withSystemTempDirectory "tempHome" $ \tempHome ->
    runConfigTests tempHome testGroup

runConfigTests :: FilePath -> Group -> IO ()
runConfigTests tempPath group = do
    defaultHome <- getEnv "HOME"
    putStrLn $ "Remeber current HOME envirement as " ++ defaultHome
    let prepare = do
          putStrLn "Set HOME envirement to 'tempHome' for tests' evalution"
          setEnv "HOME" tempPath
    -- Release resources.
    let finish _ = do
          setEnv "HOME" defaultHome
          putStrLn $ "Revert HOME envirement back to " ++ defaultHome
          putHomeEnv
    bracket prepare finish $ \_ -> void $ checkSequential group

putHomeEnv :: IO ()
putHomeEnv = do
    home <- getEnv "HOME"
    putStrLn $ concat ["Current HOME env is ", home, "\n"]

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
