{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Agenda
    , Note
    , cmdAgenda
    , cmdDone
    , cmdNew
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import qualified CRDT.LWW as LWW
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time (getCurrentTime, utctDay)
import           Data.Traversable (for)

import           FF.Options (New (New), newEnd, newStart, newText)
import           FF.Storage (DocId, Storage, list, load, save, saveNew)
import           FF.Types (Agenda, Note (Note), NoteView,
                           Status (Active, Archived), end, start, status, text)

cmdAgenda :: Storage Agenda
cmdAgenda = do
    docs <- list
    mnotes <- for docs $ \doc -> do
        mnote <- load doc
        let noteView = case mnote of
                Just Note{status = (LWW.query -> Active), text} ->
                    Just $ LWW.query text
                _ -> Nothing
        pure (doc, noteView)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]

cmdNew :: New -> Storage (DocId Note, NoteView)
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA (liftIO $ utctDay <$> getCurrentTime) newStart
    note <- lift $ do
        status  <- LWW.initial Active
        text    <- LWW.initial newText
        start   <- LWW.initial newStart'
        end     <- LWW.initial newEnd
        pure Note{..}
    nid <- saveNew note
    pure (nid, newText)

cmdDone :: DocId Note -> Storage Text
cmdDone nid = do
    mnote <- load nid
    let note@Note{text, status} =
            fromMaybe
                (error $ concat
                    [ "Can't load document "
                    , show nid
                    , ". Where did you get this id?"
                    ])
                mnote
    status' <- lift $ LWW.assign Archived status
    save nid note{status = status'}

    pure $ LWW.query text

fromMaybeA :: Applicative m => m a -> Maybe a -> m a
fromMaybeA m = maybe m pure
