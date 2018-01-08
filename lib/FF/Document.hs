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

newtype DocId doc = DocId FilePath
    deriving (Eq, Ord, ToJSONKey)

instance Show (DocId doc) where
    show (DocId path) = path

list :: forall doc. Collection doc => FilePath -> IO [DocId doc]
list dataDir = map DocId <$> listDirectory (dataDir </> collectionName @doc)

load :: forall doc. Collection doc => FilePath -> DocId doc -> IO (Maybe doc)
load dataDir (DocId docId) = do
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

save
    :: forall doc
    . Collection doc => FilePath -> DocId doc -> doc -> LamportClock ()
save dataDir (DocId docId) doc = do
    version <- getTime
    let versionFile = docDir </> lamportTimeToFileName version
    liftIO $ do
        createDirectoryIfMissing True docDir
        BSL.writeFile versionFile $ encode doc
  where
    docDir = dataDir </> collectionName @doc </> docId

saveNew :: Collection doc => FilePath -> doc -> LamportClock (DocId doc)
saveNew dataDir doc = do
    docId <- DocId . lamportTimeToFileName <$> getTime
    save dataDir docId doc
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
