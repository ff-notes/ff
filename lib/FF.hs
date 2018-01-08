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
import           CRDT.LamportClock (LamportClock, LamportTime (LamportTime),
                                    Pid)
import           CRDT.LWW (LWW (LWW))
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, ToJSON, Value (Array), parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import           Data.Traversable (for)
import           GHC.Exts (fromList)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))

import           FF.Document (DocId (DocId), loadDocument, saveDocument,
                              saveNewDocument)

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
    }
    deriving (Eq, Show)

instance Semigroup Note where
    Note alive1 text1 <> Note alive2 text2 =
        Note (alive1 <> alive2) (text1 <> text2)

instance Semilattice Note

deriveJSON defaultOptions ''Note

type NoteView = Text

type Agenda = Map DocId NoteView

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

cmdNew :: FilePath -> Text -> LamportClock DocId
cmdNew dataDir content = do
    let notesDir = dataDir </> "note"
    status <- Just <$> LWW.initial Active
    text <- LWW.initial content
    saveNewDocument notesDir Note{status, text}

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
