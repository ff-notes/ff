{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad.Trans (lift)
import           CRDT.LamportClock (Clock, Pid (..), Process)
import           CRDT.LamportClock.Simulation (ProcessSimT, runLamportClockSimT,
                                               runProcessSimT)
import           Data.Foldable (for_)
import           System.Directory (copyFile, createDirectory,
                                   doesDirectoryExist, doesFileExist,
                                   listDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)

import           FF.Storage (MonadStorage (..), Storage, runStorage)
import qualified FF.Storage as Storage

import           FF.Upgrade (upgradeDatabase)

main :: IO ()
main =
    withSystemTempDirectory "TestJson2Ron" $ \hDataDir -> do
        copyContent "TestJson2Ron.in" hDataDir
        hClock <- newTVarIO 0
        runStorageSim Storage.Handle{..} upgradeDatabase
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

newtype StorageSim a = StorageSim (ProcessSimT Storage a)
    deriving (Applicative, Clock, Functor, Monad, Process)

instance MonadStorage StorageSim where
    listCollections = StorageSim $ lift listCollections
    listDirectoryIfExists relpath =
        StorageSim . lift $ listDirectoryIfExists relpath
    createFile docId time doc =
        StorageSim . lift $ createFile docId time doc
    readFileEither docId version =
        StorageSim . lift $ readFileEither docId version
    removeFileIfExists docId version =
        StorageSim . lift $ removeFileIfExists docId version

runStorageSim :: Storage.Handle -> StorageSim a -> IO a
runStorageSim h (StorageSim action) =
    either fail pure =<<
    runStorage h (runLamportClockSimT (runProcessSimT (Pid 42) action))
