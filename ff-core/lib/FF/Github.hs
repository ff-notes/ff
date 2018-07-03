{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    ) where

import           Control.Arrow ((&&&))
import           Control.Error (failWith)
import           Control.Monad (unless)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           GitHub (FetchCount (..), Id, Issue (..), IssueState (..),
                         Milestone (..), URL (..), executeRequest',
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle, mkOwnerName, mkRepoName,
                         untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

runCmdGithub
    :: Maybe Text
    -> Maybe Limit
    -> Day  -- ^ today
    -> ExceptT Text IO (ModeMap Sample)
runCmdGithub mAddress mlimit today = do
    address <- case mAddress of
        Just address -> do
            unless (Text.length (Text.filter (=='/') address) == 1
                && not ("/" `Text.isPrefixOf` address)
                && not ("/" `Text.isSuffixOf` address))
                (throwError $ Text.concat
                    [ "Something is wrong with "
                    , address
                    , ". Please, check correctness of input. "
                    , "Right format is OWNER/REPO"
                    ])
            pure address
        Nothing -> do
            packed <- liftIO $ Text.pack
                <$> readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            let mGithub = Text.stripSuffix ".git\n"
                    =<< Text.stripPrefix "https://github.com/" packed
            failWith "Sorry, only github repository expected." mGithub
    let (owner, repo) = Text.takeWhile (/='/') &&& Text.takeWhileEnd (/='/') $ address
    let fetching = maybe FetchAll (FetchAtLeast . fromIntegral) mlimit
    let request = issuesForRepoR (mkOwnerName owner) (mkRepoName repo) mempty fetching
    response <- withExceptT (Text.pack . show) (ExceptT $ executeRequest' request)
    pure $ sampleMaps mlimit today response

sampleMaps :: Foldable t => Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map toNoteView
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

toNoteView :: Issue -> NoteView
toNoteView Issue{..} = NoteView
    { nid    = toNoteId issueId
    , status = toStatus issueState
    , text   = issueTitle <> maybeUrl
    , start  = utctDay issueCreatedAt
    , end    = maybeMilestone
    }
  where
    maybeUrl = case issueHtmlUrl of
        Just (URL url) -> "\nurl " <> url
        Nothing        -> ""
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId = DocId . show . untagId

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
