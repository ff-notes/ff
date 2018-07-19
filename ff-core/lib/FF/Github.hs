{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdTrack
    , sampleMaps
    ) where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           GitHub (FetchCount (..), Issue (..), IssueState (..),
                         Milestone (..), URL (..), executeRequest',
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle, mkOwnerName, mkRepoName)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Options (Track (..))
import           FF.Types (Limit, ModeMap, NoteView (..), Sample (..),
                           Status (..), Tracked (..))

runCmdTrack
    :: Track
    -> Day  -- ^ today
    -> ExceptT Text IO (ModeMap Sample)
runCmdTrack Track{trackAddress, trackLimit} today = do
    address <- case trackAddress of
        Just address -> pure address
        Nothing -> do
            packed <- liftIO $ Text.pack <$>
                readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            failWith "Sorry, only github repository expected." $
                Text.stripPrefix "https://github.com/" packed
                >>= Text.stripSuffix ".git\n"
    (owner, repo) <- case Text.splitOn "/" address of
        [owner, repo] | not $ Text.null owner, not $ Text.null repo ->
            pure (owner, repo)
        _ -> throwError $
            "Something is wrong with " <> address <>
            ". Please, check correctness of input. Right format is OWNER/REPO"
    response <- withExceptT (Text.pack . show) $ ExceptT $
        executeRequest' $ issuesForRepoR
            (mkOwnerName owner)
            (mkRepoName repo)
            mempty
            (maybe FetchAll (FetchAtLeast . fromIntegral) trackLimit)
    pure $ sampleMaps address trackLimit today response

sampleMaps
    :: Foldable t => Text -> Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps address mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map (toNoteView address)
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

toNoteView :: Text -> Issue -> NoteView
toNoteView address Issue{..} = NoteView
    { nid     = Nothing
    , status  = toStatus issueState
    , text    = issueTitle
    , start   = utctDay issueCreatedAt
    , end     = maybeMilestone
    , tracked = Just Tracked
        { trackedProvider   = "github"
        , trackedSource     = address
        , trackedExternalId
        , trackedUrl
        }
    }
  where
    trackedExternalId = Text.pack $ show issueNumber
    trackedUrl = case issueHtmlUrl of
        Just (URL url) -> url
        Nothing        ->
            "https://github.com/" <> address <> "/issues/" <> trackedExternalId
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
