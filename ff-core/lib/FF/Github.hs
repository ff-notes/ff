{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( getIssueViews
    , getIssueSamples
    , sampleMaps
    ) where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           Data.Vector (Vector)
-- import           GitHub (FetchCount (..), Issue (..), IssueState (..),
--                          Milestone (..), executeRequest', getUrl,
import           GitHub (FetchCount (..), Issue (..), IssueState (..),
                         Milestone (..), URL (..), executeRequest',
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle, mkOwnerName, mkRepoName)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
-- import           FF.Options (Track (..))
import           FF.Types (Limit, ModeMap, NoteView (..), Sample (..),
                           Status (..), Tracked (..))

getIssues
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO (Vector Issue)
getIssues mAddress mlimit = do
    address <- case mAddress of
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
    withExceptT (Text.pack . show) $ ExceptT $
        executeRequest' $ issuesForRepoR
            (mkOwnerName owner)
            (mkRepoName repo)
            mempty
            (maybe FetchAll (FetchAtLeast . fromIntegral) mlimit)

getIssueSamples
    :: Maybe Text
    -> Maybe Limit
    -> Day
    -> ExceptT Text IO (ModeMap Sample)
getIssueSamples mAddress mlimit today =
    sampleMaps mAddress mlimit today <$> getIssues mAddress mlimit

getIssueViews
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO [NoteView]
getIssueViews mAddress mlimit =
    noteViewList mAddress mlimit <$> getIssues mAddress mlimit

sampleMaps
    :: Foldable t => Maybe Text -> Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps address mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map (issueToNoteView address)
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

noteViewList :: Foldable t => Maybe Text -> Maybe Limit -> t Issue -> [NoteView]
noteViewList address mlimit issues =
    map (issueToNoteView address)
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

issueToNoteView :: Maybe Text -> Issue -> NoteView
issueToNoteView mAddress Issue{..} = NoteView
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
    address = fromMaybe "{noRepo}" mAddress
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
