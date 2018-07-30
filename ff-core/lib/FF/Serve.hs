{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module FF.Serve
    ( cmdServe
    ) where

import           Prelude hiding (div, span)

import           Control.Monad.Extra (whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default.Class (def)
import           Data.Function ((&))
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Network.Wai.Handler.Warp (defaultSettings, setHost)
import           System.IO (hPutStrLn, stderr)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 (Html, a, br, div, h1, li, p, section, span,
                                   stringValue, strong, style, toHtml, ul, (!))
import           Text.Blaze.Html5.Attributes (class_, href)
import           Web.Scotty (get, html, scottyOpts, settings, verbose)

import           FF (getSamples, getUtcToday)
import           FF.Config (ConfigUI (..))
import           FF.Storage (runStorage)
import qualified FF.Storage as Storage
import           FF.Types (ModeMap, NoteView (..), Sample (..), TaskMode (..),
                           Tracked (..), omitted)

cmdServe :: MonadIO m => Storage.Handle -> ConfigUI -> m ()
cmdServe h ui = liftIO $ do
    hPutStrLn stderr "serving at http://localhost:3000/"
    scottyOpts opts $ get "/" $ do
        today <- getUtcToday
        nvs <- liftIO $ runStorage h $ getSamples ui Nothing today
        html $ renderHtml $ do
            style ".metaItem { color: #ccc; }"
            prettyHtmlSamplesBySections nvs
  where
    opts = def{verbose = 0, settings = defaultSettings & setHost "::1"}

prettyHtmlSamplesBySections :: ModeMap Sample -> Html
prettyHtmlSamplesBySections samples = do
    mconcat [prettyHtmlSample mode sample | (mode, sample) <- Map.assocs samples]
    mconcat [p $ toHtml numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettyHtmlSample :: TaskMode -> Sample -> Html
prettyHtmlSample mode = \case
    Sample{total = 0} -> mempty
    Sample{notes} ->
        section $ do
            h1 $ toHtml (labels mode)
            ul $ mconcat $ map noteView notes
  where
    metaItem k v = span ! class_ "metaItem" $ " | " *> strong k *> " " *> v
    noteView NoteView{..} = li $ do
        p $
            case Text.lines text of
                []            -> pure ()
                [_]           -> toHtml text
                header : body ->
                    mconcat $
                    intersperse br $
                    strong (toHtml header) : map toHtml body
        div $ do
            whenJust nid $ \i -> metaItem "id" $ toHtml $ show i
            metaItem "start" $ toHtml $ show start
            whenJust tracked $ \Tracked{..} -> do
                metaItem "tracking" $ toHtml trackedSource
                metaItem "url" $
                    a ! href (stringValue $ Text.unpack trackedUrl) $
                        toHtml trackedUrl
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
