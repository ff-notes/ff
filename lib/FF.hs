{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Agenda
    , Note
    , cmdAgenda
    , cmdDone
    , cmdNew
    ) where

import           Control.Monad.IO.Class (liftIO)
import           CRDT.LamportClock (LamportClock, LamportTime (LamportTime),
                                    Pid)
import           CRDT.LWW (LWW (LWW), time, value)
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
import           Data.Time.Calendar (Day)
import           Data.Traversable (for)
import           GHC.Exts (fromList)

import           FF.Document (Collection, DocId, collectionName, list, load,
                              save, saveNew)

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

instance Collection Note where
    collectionName = "note"

type NoteView = Text

type Agenda = Map (DocId Note) NoteView

cmdAgenda :: FilePath -> IO Agenda
cmdAgenda dataDir = do
    docs <- list dataDir
    mnotes <- for docs $ \doc -> do
        mnote <- load dataDir doc
        let noteView = case mnote of
                Just Note{status = Nothing, text} -> Just $ LWW.query text
                Just Note{status = Just (LWW.query -> Active), text} ->
                    Just $ LWW.query text
                _ -> Nothing
        pure (doc, noteView)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]

cmdNew :: FilePath -> Text -> Maybe Day -> Maybe Day -> LamportClock (DocId Note)
cmdNew dataDir content start end = do
    status <- Just <$> LWW.initial Active
    text <- LWW.initial content
    startDate <- LWW.initial start
    endDate <- LWW.initial end
    saveNew dataDir Note{status, text, startDate, endDate}

cmdDone :: FilePath -> DocId Note -> LamportClock Text
cmdDone dataDir noteId = do
    mnote <- liftIO $ load dataDir noteId
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
    save dataDir noteId note{status = Just status'}

    pure $ LWW.query $ text note
