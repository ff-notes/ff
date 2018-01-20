{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Agenda
    , Note
    , getAgenda
    , cmdDone
    , cmdNew
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import qualified CRDT.LWW as LWW
import           Data.List (genericLength, sortOn)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time (getCurrentTime, utctDay)
import           Data.Traversable (for)

import           FF.Options (New (New), newEnd, newStart, newText)
import           FF.Storage (DocId, Storage, list, load, save, saveNew)
import           FF.Types (Agenda (..), Note (..), NoteView (..),
                           Status (Active, Archived), noteView)

getAgenda :: Maybe Int -> Storage Agenda
getAgenda mlimit = do
    docs <- list
    mnotes <- for docs load
    let allNotes =
            sortOn
                (\NoteView{start, _id} -> (start, _id))
                [ noteView doc note
                | (doc, Just note@Note{status = (LWW.query -> Active)}) <-
                    zip docs mnotes
                ]
    pure Agenda
        { notes = case mlimit of
            Nothing    -> allNotes
            Just limit -> take limit allNotes
        , total = genericLength allNotes
        }

cmdNew :: New -> Storage NoteView
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA (liftIO $ utctDay <$> getCurrentTime) newStart
    note <- lift $ do
        status  <- LWW.initial Active
        text    <- LWW.initial newText
        start   <- LWW.initial newStart'
        end     <- LWW.initial newEnd
        pure Note{..}
    nid <- saveNew note
    pure $ noteView nid note

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
