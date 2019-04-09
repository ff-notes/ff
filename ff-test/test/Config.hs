{-# LANGUAGE OverloadedStrings #-}

module Config (configTests) where

import           Control.Exception (bracket)
import           Control.Monad (void)
import           FF.CLI (runCmdConfig)
import           FF.Config (Config (..), defaultConfigUI, emptyConfig,
                            loadConfig, printConfig, saveConfig)
import           Hedgehog (Group (..), Property, checkSequential, evalIO,
                           property, (===))
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

prop_loadNoConfig :: Property
prop_loadNoConfig = property $ do
        conf <- evalIO loadConfig
        emptyConfig === conf

prop_loadJustConfig :: Property
prop_loadJustConfig = property $ do
        let setConfig = Config (Just "pathNotes") defaultConfigUI
        evalIO $ saveConfig setConfig
        loadedconf <- evalIO loadConfig
        setConfig === loadedconf

prop_loadNothingConfig :: Property
prop_loadNothingConfig = property $ do
        let setConfig = Config Nothing defaultConfigUI
        evalIO $ saveConfig setConfig
        loadedconf <- evalIO loadConfig
        setConfig === loadedconf

prop_runCmdConfigJust :: Property
prop_runCmdConfigJust = property $ do
        let setConfig = Config (Just "pathNotes") defaultConfigUI
        evalIO $ saveConfig setConfig
        runcmdconf <- evalIO $ hCapture_ [stderr]$ runCmdConfig setConfig Nothing
        printsetconf <- evalIO $ hCapture_ [stderr] $ printConfig setConfig
        runcmdconf === printsetconf

prop_runCmdConfigNothing :: Property
prop_runCmdConfigNothing = property $ do
        let setConfig = Config Nothing defaultConfigUI
        evalIO $ saveConfig setConfig
        runcmdconf <- evalIO $ hCapture_ [stderr]$ runCmdConfig setConfig Nothing
        printsetconf <- evalIO $ hCapture_ [stderr] $ printConfig setConfig
        runcmdconf === printsetconf

testGroup :: Group
testGroup = Group "config tests" [
        ("LoadConfig with an absent config file:", prop_loadNoConfig),
        ("LoadConfig with defined dataDir:", prop_loadJustConfig),
        ("LoadConfig with Nothing in dataDir field:", prop_loadNothingConfig),
        ("RunCmdConf with defined dataDir:", prop_runCmdConfigJust),
        ("RunCmdConf with Nothing in dataDir field:", prop_runCmdConfigNothing)
        ]
