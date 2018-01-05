{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF
    (
    -- * Agenda
      Agenda
    , cmdAgenda
    -- * New
    , cmdNew
    ) where

import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportTime (LamportTime), Pid (Pid),
                                    getRealLamportTime)
import           CRDT.LWW (LWW (LWW))
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, ToJSON, Value (Array), eitherDecode,
                             encode, parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (toList)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, sconcat, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           GHC.Exts (fromList)
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>))

instance FromJSON a => FromJSON (LWW a) where
    parseJSON (Array a) = case toList a of
        [valueJ, timeJ, pidJ] -> LWW <$> parseJSON valueJ <*> parseTime
          where
            parseTime = LamportTime <$> parseJSON timeJ <*> parsePid
            parsePid = Pid <$> parseJSON pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]
    parseJSON v = typeMismatch "Array" v

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time (Pid pid)} =
        Array $ fromList [toJSON value, toJSON time, toJSON pid]

data Status = Active | Archived | Deleted
    deriving (Eq, Show)

deriveJSON defaultOptions ''Status

data Note = Note
    { status :: !(Maybe (LWW Status))
    , text   :: !(LWW Text)
    }
    deriving (Eq, Show)

instance Semigroup Note where
    Note alive1 text1 <> Note alive2 text2 =
        Note (alive1 <> alive2) (text1 <> text2)

instance Semilattice Note

deriveJSON defaultOptions ''Note

type NoteId = Text

type NoteView = Text

type Agenda = Map NoteId NoteView

loadDocument
    :: (CvRDT doc, FromJSON doc) => FilePath -> FilePath -> IO (Maybe doc)
loadDocument dir doc = do
    versionFiles <- listDirectory $ dir </> doc
    versions <- for versionFiles $ \version -> do
        let versionPath = dir </> doc </> version
        contents <- BS.readFile versionPath
        pure $
            either (error . ((versionPath ++ ": ") ++)) id $
            eitherDecode contents
    pure $ sconcat <$> nonEmpty versions

cmdAgenda :: FilePath -> IO Agenda
cmdAgenda dataDir = do
    let notesDir = dataDir </> "note"
    files <- listDirectory notesDir
    mnotes <- for files $ \name -> do
        note <- loadDocument notesDir name
        let noteView = case note of
                Just Note{status = Nothing, text} ->
                    Just $ LWW.query text
                Just Note{status = Just status, text}
                    | LWW.query status == Active -> Just $ LWW.query text
                _ -> Nothing
        pure (Text.pack name, noteView)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]

cmdNew :: FilePath -> Text -> IO NoteId
cmdNew dataDir content = do
    let notesDir = dataDir </> "note"
    noteTime <- getRealLamportTime
    let noteId = show noteTime
    let noteDir = notesDir </> noteId
    version <- getRealLamportTime
    let versionFile = noteDir </> show version

    status <- Just <$> lwwIitial' Active
    text <- lwwIitial' content

    createDirectoryIfMissing True noteDir
    BS.writeFile versionFile $ encode Note{status, text}

    pure $ Text.pack noteId

lwwIitial' :: a -> IO (LWW a)
lwwIitial' value = LWW value <$> getRealLamportTime
