{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module FF.Serve
    ( cmdServe ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import           Web.Scotty (scotty, get, html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           FF (getUtcToday, getSamples)
import           FF.Storage (runStorage)
import qualified FF.Storage as Storage
import           FF.Config (ConfigUI (..))
import           FF.Types (ModeMap, Sample (..), TaskMode (..), omitted)


serveHttpPort :: Int
serveHttpPort = 8080

cmdServe :: MonadIO m => Storage.Handle -> ConfigUI -> m ()
cmdServe h ui =
    liftIO $ scotty serveHttpPort $ get "/" $ do
        today <- getUtcToday
        nvs <- liftIO $ runStorage h $ getSamples ui Nothing today
        html $ renderHtml $ prettyHtmlSamplesBySections nvs

prettyHtmlSamplesBySections :: ModeMap Sample -> H.Html
prettyHtmlSamplesBySections samples = do
    mconcat [prettyHtmlSample mode sample | (mode, sample) <- Map.assocs samples]
    mconcat [H.p $ H.toHtml numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettyHtmlSample :: TaskMode -> Sample -> H.Html
prettyHtmlSample mode = \case
    Sample{total = 0} -> mempty
    Sample{total, notes} ->
        H.div $ do
            H.h1 $ H.toHtml (labels mode)
            H.ul $ mconcat $ map (H.li . H.toHtml . show) notes
  where
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

