{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade (upgradeDatabase) where

import           Data.Foldable (for_)

import           FF.Storage (Collection, MonadStorage (..), listDocuments,
                             modify)
import           FF.Types (Note)

upgradeDatabase :: MonadStorage m => m ()
upgradeDatabase = do
    collections <- listCollections
    for_ collections $ \case
        "note"      -> upgradeCollection @Note
        collection  -> fail $ "unsupported type " ++ show collection

upgradeCollection
    :: forall doc m . (Collection doc, Eq doc, MonadStorage m) => m ()
upgradeCollection = do
    docs <- listDocuments @_ @doc
    for_ docs $ \docId ->
        modify docId $ \case
            Nothing  -> fail $ "Can't load document " ++ show docId
            Just doc -> pure ((), doc)
