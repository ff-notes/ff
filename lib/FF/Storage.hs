{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Storage where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Trans (lift)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportClock, LamportTime (LamportTime),
                                    LocalTime, Pid (Pid), getTime,
                                    runLamportClock)
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (chr, ord)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Semigroup (sconcat)
import           Data.Traversable (for)
import           Numeric (showIntAtBase)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory)
import           System.FilePath ((</>))

class (CvRDT doc, FromJSON doc, ToJSON doc) => Collection doc where
    collectionName :: FilePath

newtype DocId doc = DocId FilePath
    deriving (Eq, Ord, ToJSON, ToJSONKey)

instance Show (DocId doc) where
    show (DocId path) = path

-- | Environment is the dataDir
type Storage = ReaderT FilePath LamportClock

runStorage :: FilePath -> TVar LocalTime -> Storage a -> IO a
runStorage dataDir var action = runLamportClock var $ runReaderT action dataDir

list :: forall doc. Collection doc => Storage [DocId doc]
list = do
    dataDir <- ask
    map DocId
        <$> liftIO (listDirectoryIfExists $ dataDir </> collectionName @doc)

load :: forall doc. Collection doc => DocId doc -> Storage (Maybe doc)
load docId = do
    docDir <- askDocDir docId
    liftIO $ do
        versionFiles <- listDirectoryIfExists docDir
        versions <- for versionFiles $ \version -> do
            let versionPath = docDir </> version
            contents <- BSL.readFile versionPath
            pure $
                either (error . ((versionPath ++ ": ") ++)) id $
                eitherDecode contents
        pure $ sconcat <$> nonEmpty versions

askDocDir :: forall doc. Collection doc => DocId doc -> Storage FilePath
askDocDir (DocId docId) = asks (</> collectionName @doc </> docId)

save :: forall doc. Collection doc => DocId doc -> doc -> Storage ()
save docId doc = do
    docDir <- askDocDir docId
    version <- lift getTime
    let versionFile = docDir </> lamportTimeToFileName version
    liftIO $ do
        createDirectoryIfMissing True docDir
        BSL.writeFile versionFile $ encode doc

saveNew :: Collection doc => doc -> Storage (DocId doc)
saveNew doc = do
    docId <- DocId . lamportTimeToFileName <$> lift getTime
    save docId doc
    pure docId

showBase36 :: (Integral a, Show a) => a -> String -> String
showBase36 = showIntAtBase 36 intToDigit36

intToDigit36 :: Int -> Char
intToDigit36 i
    | (i >=  0) && (i <=  9) = chr (ord '0'      + i)
    | (i >= 10) && (i <= 35) = chr (ord 'a' - 10 + i)
    | otherwise              = error ("not a digit " ++ show i)

lamportTimeToFileName :: LamportTime -> FilePath
lamportTimeToFileName (LamportTime time (Pid pid)) =
    showBase36 time $ '-' : showBase36 pid ""

listDirectoryIfExists :: FilePath -> IO [FilePath]
listDirectoryIfExists dir = do
    exists <- doesDirectoryExist dir
    if exists then listDirectory dir else pure []
