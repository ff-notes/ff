{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FF
    ( Agenda
    , cmdAgenda
    ) where

import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportTime, Pid)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, eitherDecode)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as BS
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, sconcat)
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))

deriveJSON defaultOptions ''LamportTime
deriveJSON defaultOptions ''LWW
deriveJSON defaultOptions ''Pid

newtype Note = Note (LWW Text)
    deriving (Eq, Semigroup, Semilattice, Show)

deriveJSON defaultOptions ''Note

type NoteId = Text

type NoteView = Text

type Agenda = Map NoteId NoteView

noteView :: Note -> NoteView
noteView (Note lww) = LWW.query lww

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
        pure (Text.pack name, noteView <$> note)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]
