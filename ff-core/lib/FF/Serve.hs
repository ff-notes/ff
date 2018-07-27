{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module FF.Serve
    ( cmdServe ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Web.Scotty (scotty, get, html)
import           Text.Blaze.Html (stringValue)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           FF (getUtcToday, getSamples)
import           FF.Storage (runStorage)
import qualified FF.Storage as Storage
import           FF.Config (ConfigUI (..))
import           FF.Types (NoteView(..), ModeMap, Sample (..), TaskMode (..), Tracked(..),  omitted)


serveHttpPort :: Int
serveHttpPort = 8080

cmdServe :: MonadIO m => Storage.Handle -> ConfigUI -> m ()
cmdServe h ui =
    liftIO $ scotty serveHttpPort $ get "/" $ do
        today <- getUtcToday
        nvs <- liftIO $ runStorage h $ getSamples ui Nothing today
        html $ renderHtml $ do
            H.meta H.! A.charset "utf-8"
            H.style ".nfo-item * { margin: 2px; }"
            prettyHtmlSamplesBySections nvs

prettyHtmlSamplesBySections :: ModeMap Sample -> H.Html
prettyHtmlSamplesBySections samples = do
    mconcat [prettyHtmlSample mode sample | (mode, sample) <- Map.assocs samples]
    mconcat [H.p $ H.toHtml numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettyHtmlSample :: TaskMode -> Sample -> H.Html
prettyHtmlSample mode = \case
    Sample{total = 0} -> mempty
    Sample{notes} ->
        H.div $ do
            H.h1 $ H.toHtml (labels mode)
            H.ul $ mconcat $ map fmtNote notes
    where
    fmtNote (NoteView nid _ text start _ tracked) =
        H.li $ do
            H.h3 $ H.toHtml text
            when (isJust nid) $ H.div H.! A.class_ "nfo-item" $ do
                H.b "id"
                H.toHtml $ show $ fromJust nid
            H.div H.! A.class_ "nfo-item" $ do
                H.b "start"
                H.toHtml $ show start
            when (isJust tracked) $ do
                H.div H.! A.class_ "nfo-item" $ do
                    H.b "tracking"
                    H.toHtml $ trackedSource $ fromJust tracked
                H.div H.! A.class_ "nfo-item" $ do
                    let url = trackedUrl $ fromJust tracked
                    H.b "url"
                    H.a H.! A.href (stringValue $ Text.unpack url) $ H.toHtml url
    labels = \case
        Overdue n -> case n of
            1 -> "1 day overdue:"
            _ -> show n <> " days overdue:"
        EndToday -> "Due today:"
        EndSoon n -> case n of
            1 -> "Due tomorrow:"
            _ -> "Due in " <> show n <> " days:"
        Actual -> "Actual:"
        Starting n -> case n of
            1 -> "Starting tomorrow:"
            _ -> "Starting in " <> show n <> " days:"

