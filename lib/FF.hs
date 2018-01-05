{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FF
    ( Result (..)
    , runAgenda
    ) where

import           CRDT.LamportClock (LamportTime, Pid)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Aeson (eitherDecode)
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import qualified Data.ByteString.Lazy as BS
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, sconcat)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))

deriveJSON defaultOptions ''LamportTime
deriveJSON defaultOptions ''LWW
deriveJSON defaultOptions ''Pid

newtype Note = Note (LWW Text)
    deriving (Eq, Semigroup, Show)

deriveJSON defaultOptions ''Note

type NoteId = Text

type NoteView = Text

newtype Result = Result
    { notes :: Map NoteId NoteView
    }
    deriving (Eq, Show)

deriveToJSON defaultOptions ''Result

noteView :: Note -> NoteView
noteView (Note lww) = LWW.query lww

loadDocument :: FilePath -> FilePath -> IO (Maybe Note)
loadDocument base doc = do
    versionFiles <- listDirectory $ base </> doc
    versions <- for versionFiles $ \version -> do
        let versionPath = base </> doc </> version
        contents <- BS.readFile versionPath
        pure $
            either (error . ((versionPath ++ ": ") ++)) id $
            eitherDecode contents
    pure $ sconcat <$> nonEmpty versions

runAgenda :: FilePath -> IO Result
runAgenda base = do
    files <- listDirectory $ base </> "note"
    mnotes <- for files $ \doc -> do
        note <- loadDocument (base </> "note") doc
        pure (Text.pack doc, noteView <$> note)
    pure Result{notes = Map.fromList [(k, note) | (k, Just note) <- mnotes]}
