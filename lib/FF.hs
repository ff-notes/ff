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
import           CRDT.LamportClock (LamportTime, Pid, getRealLamportTime)
import           CRDT.LWW (LWW (LWW))
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, eitherDecode, encode)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as BS
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, sconcat, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>))

deriveJSON defaultOptions ''LamportTime
deriveJSON defaultOptions ''LWW
deriveJSON defaultOptions ''Pid

data Note = Note
    { alive :: !(Maybe (LWW Bool)) -- ^ undone, unfinished
    , text  :: !(LWW Text)
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
                Just Note{alive = Nothing, text} ->
                    Just $ LWW.query text
                Just Note{alive = Just alive, text} | LWW.query alive ->
                    Just $ LWW.query text
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

    alive <- Just <$> lwwIitial' True
    text <- lwwIitial' content

    createDirectoryIfMissing True noteDir
    BS.writeFile versionFile $ encode Note{alive, text}

    pure $ Text.pack noteId

lwwIitial' :: a -> IO (LWW a)
lwwIitial' value = LWW value <$> getRealLamportTime
