{-# LANGUAGE AllowAmbiguousTypes #-}

module RON.Storage (Collection (..), DocId (..), MonadStorage (..)) where

import           RON.Data (Replicated)
import           RON.Event (Clock)
import           RON.Types (UUID)

type Version = UUID

newtype DocId doc = DocId UUID

type CollectionName = FilePath

class Replicated doc => Collection doc where
    collectionName :: CollectionName

class Clock m => MonadStorage m where
    listCollections :: m [CollectionName]

    -- | Must return @[]@ for non-existent collection
    listDocuments :: Collection doc => m [DocId doc]

    -- | Must return @[]@ for non-existent document
    listVersions :: Collection doc => DocId doc -> m [Version]

    -- | Must create collection and document if not exist
    createVersion :: Collection doc => DocId doc -> Version -> doc -> m ()

    readVersion
        :: Collection doc => DocId doc -> Version -> m (Either String doc)

    deleteVersion :: Collection doc => DocId doc -> Version -> m ()
