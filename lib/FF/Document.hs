{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Document where

import           Control.Monad.IO.Class (liftIO)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportClock, LamportTime (LamportTime),
                                    Pid (Pid), getTime)
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (chr, ord)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Semigroup (sconcat)
import           Data.Traversable (for)
import           Numeric (showIntAtBase)
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>))

class (CvRDT doc, FromJSON doc, ToJSON doc) => Collection doc where
    collectionName :: FilePath

newtype DocId = DocId FilePath
    deriving (Eq, Ord, ToJSONKey)

instance Show DocId where
    show (DocId path) = path

loadDocument
    :: forall doc. Collection doc => FilePath -> DocId -> IO (Maybe doc)
loadDocument dataDir (DocId docId) = do
    versionFiles <- listDirectory docDir
    versions <- for versionFiles $ \version -> do
        let versionPath = docDir </> version
        contents <- BSL.readFile versionPath
        pure $
            either (error . ((versionPath ++ ": ") ++)) id $
            eitherDecode contents
    pure $ sconcat <$> nonEmpty versions
  where
    docDir = dataDir </> collectionName @doc </> docId

saveDocument
    :: forall doc. Collection doc => FilePath -> DocId -> doc -> LamportClock ()
saveDocument dataDir (DocId docId) doc = do
    version <- getTime
    let versionFile = docDir </> lamportTimeToFileName version
    liftIO $ do
        createDirectoryIfMissing True docDir
        BSL.writeFile versionFile $ encode doc
  where
    docDir = dataDir </> collectionName @doc </> docId

saveNewDocument
    :: Collection doc => FilePath -> doc -> LamportClock DocId
saveNewDocument dataDir doc = do
    docId <- DocId . lamportTimeToFileName <$> getTime
    saveDocument dataDir docId doc
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
