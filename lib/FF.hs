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
import           Data.Aeson.TH (defaultOptions, deriveJSON)
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

data Result = Result
    { notes :: Map NoteId NoteView
    }
    deriving (Eq, Show)

noteView :: Note -> NoteView
noteView (Note lww) = LWW.query lww

runAgenda :: FilePath -> IO Result
runAgenda base = do
    files <- listDirectory $ base </> "note"
    mnotes <- for files $ \doc -> do
        versionFiles <- listDirectory $ base </> "note" </> doc
        versions <- for versionFiles $ \version -> do
            let versionPath = base </> "note" </> doc </> version
            contents <- BS.readFile versionPath
            let note =
                    either (error . ((versionPath ++ ": ") ++)) id $
                    eitherDecode contents
            pure note
        pure (Text.pack doc, noteView . sconcat <$> nonEmpty versions)
    pure Result{notes = Map.fromList [(k, note) | (k, Just note) <- mnotes]}
