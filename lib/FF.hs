{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF
    ( Agenda
    , DocId (..)
    , cmdAgenda
    , cmdDone
    , cmdNew
    ) where

import           Control.Monad.IO.Class (liftIO)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportClock, LamportTime (LamportTime),
                                    Pid (..), getTime)
import           CRDT.LWW (LWW (LWW))
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, Value (Array),
                             eitherDecode, encode, parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BS
import           Data.Char (chr, ord)
import           Data.Foldable (toList)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup, sconcat, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Traversable (for)
import           GHC.Exts (fromList)
import           Numeric (showIntAtBase)
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>))

deriveJSON defaultOptions ''Pid

instance FromJSON a => FromJSON (LWW a) where
    parseJSON (Array a) = case toList a of
        [valueJ, timeJ, pidJ] -> LWW <$> parseJSON valueJ <*> parseTime
          where
            parseTime = LamportTime <$> parseJSON timeJ <*> parseJSON pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]
    parseJSON v = typeMismatch "Array" v

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time pid} =
        Array $ fromList [toJSON value, toJSON time, toJSON pid]

data Status = Active | Archived | Deleted
    deriving (Eq, Show)

deriveJSON defaultOptions ''Status

data Note = Note
    { status :: !(Maybe (LWW Status))
    , text   :: !(LWW Text)
    , startDate :: !(LWW (Maybe Day))
    , endDate :: !(LWW (Maybe Day))
    }
    deriving (Eq, Show)

instance Semigroup Note where
    Note alive1 text1 s1 e1 <> Note alive2 text2 s2 e2 =
        Note (alive1 <> alive2) (text1 <> text2) (s1 <> s2) (e1 <> e2)

instance Semilattice Note

deriveJSON defaultOptions ''Note

newtype DocId = DocId FilePath
    deriving (Eq, Ord, ToJSONKey)

-- TODO(cblp, 2018-01-05) deriving via GNTD
instance Show DocId where
    show (DocId path) = path

type NoteView = Text

type Agenda = Map DocId NoteView

loadDocument
    :: (CvRDT doc, FromJSON doc) => FilePath -> DocId -> IO (Maybe doc)
loadDocument dir (DocId doc) = do
    versionFiles <- listDirectory $ dir </> doc
    versions <- for versionFiles $ \version -> do
        let versionPath = dir </> doc </> version
        contents <- BS.readFile versionPath
        pure $
            either (error . ((versionPath ++ ": ") ++)) id $
            eitherDecode contents
    pure $ sconcat <$> nonEmpty versions

saveDocument
    :: (CvRDT doc, ToJSON doc) => FilePath -> DocId -> doc -> LamportClock ()
saveDocument dir (DocId docId) doc = do
    let docDir = dir </> docId
    version <- getTime
    let versionFile = docDir </> lamportTimeToFileName version
    liftIO $ do
        createDirectoryIfMissing True docDir
        BS.writeFile versionFile $ encode doc

saveNewDocument
    :: (CvRDT doc, ToJSON doc) => FilePath -> doc -> LamportClock DocId
saveNewDocument dir doc = do
    docId <- DocId . lamportTimeToFileName <$> getTime
    saveDocument dir docId doc
    pure docId

cmdAgenda :: FilePath -> IO Agenda
cmdAgenda dataDir = do
    let notesDir = dataDir </> "note"
    files <- listDirectory notesDir
    mnotes <- for files $ \name -> do
        let doc = DocId name
        note <- loadDocument notesDir doc
        let noteView = case note of
                Just Note{status = Nothing, text} ->
                    Just $ LWW.query text
                Just Note{status = Just status, text}
                    | LWW.query status == Active -> Just $ LWW.query text
                _ -> Nothing
        pure (doc, noteView)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]

cmdNew :: FilePath -> Text -> Maybe Day -> Maybe Day -> LamportClock DocId
cmdNew dataDir content start end = do
    let notesDir = dataDir </> "note"
    status <- Just <$> LWW.initial Active
    text <- LWW.initial content
    startDate <- LWW.initial start
    endDate <- LWW.initial end
    saveNewDocument notesDir Note{status, text, startDate, endDate}

cmdDone :: FilePath -> DocId -> LamportClock Text
cmdDone dataDir noteId = do
    let notesDir = dataDir </> "note"
    mnote <- liftIO $ loadDocument notesDir noteId
    let note =
            fromMaybe
                (error $ concat
                    [ "Can't load document "
                    , show noteId
                    , ". Where did you get this id?"
                    ])
                mnote
    status' <- case status note of
        Nothing     -> LWW.initial  Archived
        Just status -> LWW.assign   Archived status
    saveDocument notesDir noteId note{status = Just status'}

    pure $ LWW.query $ text note

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
