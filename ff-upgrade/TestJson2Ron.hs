{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE MultiWayIf #-}

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (Pid (..))
import           CRDT.LamportClock.Simulation (LamportClockSimT (..),
                                               ProcessSimT (..),
                                               runLamportClockSimT,
                                               runProcessSimT)
import           Data.Foldable (for_)
import           System.Directory (copyFile, createDirectory,
                                   doesDirectoryExist, doesFileExist,
                                   listDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)

import           FF.Storage (StorageT, runStorageT)

import           FF.Upgrade (upgradeDatabase)

main :: IO ()
main =
    withSystemTempDirectory "TestJson2Ron" $ \hDataDir -> do
        copyContent "TestJson2Ron.in" hDataDir
        runStorageSim hDataDir upgradeDatabase
        diff "TestJson2Ron.out" hDataDir

copyContent :: FilePath -> FilePath -> IO ()
copyContent src dst = do
    content <- listDirectory src
    for_ content $ \name -> do
        let src' = src </> name
            dst' = dst </> name
        nameIsDir <- doesDirectoryExist src'
        nameIsFile <- doesFileExist src'
        if  | nameIsDir -> do
                createDirectory dst'
                copyContent src' dst'
            | nameIsFile ->
                copyFile src' dst'
            | otherwise ->
                fail "only plain files and dirs are supported"

diff :: FilePath -> FilePath -> IO ()
diff a b = callProcess "diff" ["--recursive", "--unified", a, b]

type StorageSim = StorageT (ProcessSimT IO)

runStorageSim :: FilePath -> StorageSim a -> IO a
runStorageSim dataDir action =
    runLamportClockSimT (runProcessSimT (Pid 42) $ runStorageT dataDir action)
    >>= either fail pure

instance MonadIO m => MonadIO (ProcessSimT m) where
    liftIO io = ProcessSim $ liftIO io

instance MonadIO m => MonadIO (LamportClockSimT m) where
    liftIO io = LamportClockSim $ liftIO io
