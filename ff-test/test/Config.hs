{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (configTests) where

import           Hedgehog (Gen, Property, evalIO, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Environment (setEnv)
import           System.IO (stdout)
import           System.IO.Silently (hCapture_)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           FF.CLI (runCmdConfig)
import           FF.Config (Config (Config, dataDir, ui),
                            ConfigUI (ConfigUI, shuffle), emptyConfig,
                            loadConfig, printConfig, saveConfig)

configTests :: TestTree
configTests = $(testGroupGenerator)

captureString :: IO a -> IO String
captureString = hCapture_ [stdout]

prop_runCmdConfig :: Property
prop_runCmdConfig = property $ do
    config <- forAll genConfig
    (runcmdconf, printsetconf) <-
        evalIO $ withTempHome $ do
            saveConfig config
            runcmdconf   <- captureString $ runCmdConfig config Nothing
            printsetconf <- captureString $ printConfig  config
            pure (runcmdconf, printsetconf)
    runcmdconf === printsetconf

prop_loadNoConfig :: Property
prop_loadNoConfig = property $ do
    config <- evalIO $ withTempHome loadConfig
    emptyConfig === config

prop_loadConfig :: Property
prop_loadConfig = property $ do
    config <- forAll genConfig
    loadedconf <-
        evalIO $ withTempHome $ do
            saveConfig config
            loadConfig
    config === loadedconf

genConfig :: Gen Config
genConfig = do
    dataDir <- Gen.maybe $ Gen.string (Range.linear 1 100) Gen.lower
    ui <- do
        shuffle <- Gen.bool
        pure ConfigUI{shuffle}
    pure Config{dataDir, ui}

withTempHome :: IO a -> IO a
withTempHome action =
    withSystemTempDirectory "ff-test.home" $ \tempHome -> do
        setEnv "HOME" tempHome
        action
