{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    ) where

import           Data.Foldable (toList)
import           Data.List.Split (splitOneOf)
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Data.Time (Day, UTCTime (..))
import           GitHub (Error, FetchCount (..), Id, Issue (..),
                         IssueState (..), Milestone (..), URL (..),
                         executeRequest', issueCreatedAt, issueHtmlUrl, issueId,
                         issueMilestone, issueState, issueTitle, untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readCreateProcess, shell)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

runCmdGithub
    :: Maybe String
    -> Limit
    -> Day  -- ^ today
    -> IO (Either Error (ModeMap Sample))
runCmdGithub address limit today = case address of
    Just a -> do
        let owner = head . splitOneOf "/" $ a
        let repo = last . splitOneOf "/" $ a
        let fetching = FetchAtLeast $ fromIntegral limit
        let issues = issuesForRepoR (fromString owner) (fromString repo) mempty fetching
        fmap (sampleMaps limit today) <$> executeRequest' issues
    Nothing -> do
        xs <- readCreateProcess (shell "git remote get-url --push origin") ""
        let remote = (splitOneOf "/" . drop 19 . take (length xs - 5)) xs
        let owner = head remote
        let repo = last remote
        let fetching = FetchAtLeast $ fromIntegral limit
        let issues = issuesForRepoR (fromString owner) (fromString repo) mempty fetching
        fmap (sampleMaps limit today) <$> executeRequest' issues

sampleMaps :: Foldable t => Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps limit today issues =
    takeSamples (Just limit)
    . splitModes today
    . map toNoteView
    $ take (fromIntegral limit)
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
