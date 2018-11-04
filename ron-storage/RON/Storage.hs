{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage
    ( Collection (..)
    , CollectionName
    , DocId (..)
    , Document (..)
    , DocVersion
    , MonadStorage (..)
    , createVersion
    , decodeDocId
    , loadDocument
    , modify
    , readVersion
    , uuidToFileName
    ) where

import           Control.Monad (unless, when)
import           Control.Monad.Except (MonadError, catchError, liftEither,
                                       throwError)
import           Control.Monad.State.Strict (StateT, execStateT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Traversable (for)
import           System.FilePath ((</>))

import           RON.Data (ReplicatedAsObject, reduceObject')
import           RON.Event (Clock, getEventUuid)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (Object), UUID, objectFrame, objectId)
import qualified RON.UUID as UUID

type DocVersion = FilePath

newtype DocId a = DocId FilePath

instance Collection a => Show (DocId a) where
    show (DocId file) = collectionName @a </> file

type CollectionName = FilePath

class ReplicatedAsObject a => Collection a where
    collectionName :: CollectionName
    fallbackParse :: UUID -> ByteString -> Either String (Object a)

-- | TODO rename list -> getList
class (Clock m, MonadError String m) => MonadStorage m where
    listCollections :: m [CollectionName]

    -- | Must return @[]@ for non-existent collection
    listDocuments :: Collection a => m [DocId a]

    -- | Must return @[]@ for non-existent document
    listVersions :: Collection a => DocId a -> m [DocVersion]

    -- | Must create collection and document if not exist
    saveVersionContent
        :: Collection a => DocId a -> DocVersion -> ByteString -> m ()

    loadVersionContent :: Collection a => DocId a -> DocVersion -> m ByteString

    deleteVersion :: Collection a => DocId a -> DocVersion -> m ()

    changeDocId :: Collection a => DocId a -> DocId a -> m ()

decodeDocId
    :: DocId a
    -> Maybe (Bool, UUID)  -- ^ Bool = is UUID valid
decodeDocId (DocId file) = do
    uuid <- UUID.decodeBase32 file
    pure (uuidToFileName uuid == file, uuid)

readVersion
    :: MonadStorage m
    => Collection a => DocId a -> DocVersion -> m (Object a, IsTouched)
readVersion docid version = do
    (isObjectIdValid, objectId) <-
        liftEither $
        maybe (Left $ "Bad Base32 UUID " ++ show docid) Right $
        decodeDocId docid
    unless isObjectIdValid $ throwError $ "Not a Base32 UUID " ++ show docid
    contents <- loadVersionContent docid version
    case parseStateFrame contents of
        Right objectFrame ->
            pure (Object{objectId, objectFrame}, IsTouched False)
        Left ronError -> case fallbackParse objectId contents of
            Right object       -> pure (object, IsTouched True)
            Left fallbackError -> throwError $ case BSLC.head contents of
                '{' -> fallbackError
                _   -> ronError

-- | Was fixed during loading.
-- It it was fixed during loading it must be saved to the storage.
newtype IsTouched = IsTouched Bool
    deriving Show

-- | Result of DB reading
data Document a = Document
    { value     :: Object a
        -- ^ Merged value.
    , versions  :: NonEmpty DocVersion
    , isTouched :: IsTouched
    }
    deriving Show

loadDocument :: (Collection a, MonadStorage m) => DocId a -> m (Document a)
loadDocument docid = loadRetry (3 :: Int)
  where
    loadRetry n
        | n > 0 = do
            versions0 <- listVersions docid
            case versions0 of
                []   -> throwError $ "Empty document " ++ show docid
                v:vs -> do
                    let versions = v :| vs
                    let wrapDoc (value, isTouched) =
                            Document{value, versions, isTouched}
                    e1 <-
                        for versions $ \ver -> do
                            let ctx =   "document "  ++ show docid
                                    ++  ", version " ++ ver
                                    ++  ": "
                            e1 <- try $ readVersion docid ver
                            pure $ fmapL (ctx ++) e1
                    liftEither $ wrapDoc <$> vsconcat e1
        | otherwise = throwError "Maximum retries exceeded"

-- | Validation-like version of 'sconcat'.
vsconcat
    :: NonEmpty (Either String (Object a, IsTouched))
    -> Either String (Object a, IsTouched)
vsconcat = foldr1 vappend
  where
    vappend    (Left  e1)    (Left  e2) = Left $ e1 ++ "\n" ++ e2
    vappend e1@(Left  _ )    (Right _ ) = e1
    vappend    (Right _ ) e2@(Left  _ ) = e2
    vappend    (Right r1)    (Right r2) =
        (, IsTouched (t1 || t2)) <$> reduceObject' a1 a2 where
        (a1, IsTouched t1) = r1
        (a2, IsTouched t2) = r2

try :: MonadError e m => m a -> m (Either e a)
try ma = (Right <$> ma) `catchError` (pure . Left)

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f = \case
    Left a  -> Left $ f a
    Right c -> Right c

-- TODO(2018-10-22, cblp) call `deleteVersion` from `createVersion`
modify
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Object a)
modify docid f = do
    oldDoc <- loadDocument docid
    newObj <- execStateT f $ value oldDoc
    createVersion (Just (docid, oldDoc)) newObj
    pure newObj

createVersion
    :: forall a m
    . (Collection a, MonadStorage m)
    => Maybe (DocId a, Document a) -> Object a -> m ()
createVersion mDoc newObj =
    case mDoc of
        Nothing -> save (DocId @a $ uuidToFileName objectId) []
        Just (docid, oldDoc) -> do
            let Document
                    {value = oldObj, versions, isTouched = IsTouched isTouched}
                    = oldDoc
            when (newObj /= oldObj || length versions /= 1 || isTouched) $
                save docid versions
  where
    Object{objectId, objectFrame} = newObj

    save docid oldVersions = do
        newVersion <- uuidToFileName <$> getEventUuid
        saveVersionContent docid newVersion (serializeStateFrame objectFrame)
        for_ oldVersions $ deleteVersion docid

uuidToFileName :: UUID -> FilePath
uuidToFileName = UUID.encodeBase32
