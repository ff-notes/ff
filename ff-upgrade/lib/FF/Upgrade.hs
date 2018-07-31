{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade (upgradeDatabase) where

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (filterM)
import           Control.Monad.Trans (lift)
import           CRDT.LamportClock (Clock, Pid (..), Process)
import           CRDT.LamportClock.Simulation (ProcessSimT (..),
                                               runLamportClockSimT,
                                               runProcessSimT)
import           Data.Foldable (for_)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))

import           FF.Storage (Collection, MonadStorage (..), Storage (..),
                             listDocuments, modify, runStorage)
import qualified FF.Storage as Storage
import           FF.Types (Note)

upgradeDatabase :: FilePath -> IO ()
upgradeDatabase hDataDir = do
    collections <-
        listDirectory hDataDir >>= filterM (doesDirectoryExist . (hDataDir </>))
    hClock <- newTVarIO 0
    runStorageSim Storage.Handle{..} $
        for_ collections $ \case
            "note"      -> upgradeCollection @Note
            collection  -> fail $ "unsupported type " ++ show collection

upgradeCollection
    :: forall doc m . (Collection doc, Eq doc, MonadStorage m) => m ()
upgradeCollection = do
    docs <- listDocuments @doc
    for_ docs $ \docId ->
        modify docId $ \case
            Nothing  -> fail "Can't load document"
            Just doc -> pure ((), doc)

newtype StorageWithSimClock a = StorageWithSimClock (ProcessSimT Storage a)
    deriving (Applicative, Clock, Functor, Monad, Process)

instance MonadStorage StorageWithSimClock where
    listDirectoryIfExists relpath =
        StorageWithSimClock . lift $ listDirectoryIfExists relpath
    createFile docId time doc =
        StorageWithSimClock . lift $ createFile docId time doc
    readFileEither docId version =
        StorageWithSimClock . lift $ readFileEither docId version
    removeFileIfExists docId version =
        StorageWithSimClock . lift $ removeFileIfExists docId version

runStorageSim :: Storage.Handle -> StorageWithSimClock a -> IO a
runStorageSim h (StorageWithSimClock action) =
    either fail pure =<<
    runStorage h (runLamportClockSimT (runProcessSimT (Pid 42) action))
