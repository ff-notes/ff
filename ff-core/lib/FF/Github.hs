{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.Foldable (toList)
import           Data.List (genericLength)
import           Data.Semigroup ((<>))
import           Data.Time (UTCTime (..))
import           Data.Vector (Vector)

import           FF.Storage (DocId (..))
import           FF.Types (ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..), TaskMode (..), singletonSampleMap)

import           GitHub (Issue (..), IssueState (..), Milestone (..),
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle)
import           GitHub.Data.Definitions (Error (..), Owner)
import           GitHub.Data.Id
import           GitHub.Data.Name (Name)
import           GitHub.Data.Options (stateOpen)
import           GitHub.Data.Repos (Repo)
import qualified GitHub.Data.URL as URL
import           GitHub.Endpoints.Issues (issuesForRepo)

runCmdGithub :: Name Owner -> Name Repo -> Int -> IO (Either Error (ModeMap Sample))
runCmdGithub owner repo limit = do
    possibleIssues <- issuesForRepo owner repo stateOpen
    return $ fmap (toSampleMap limit) possibleIssues

toSampleMap :: Int -> Vector Issue -> ModeMap Sample
toSampleMap limit issues = singletonSampleMap Actual sample
  where
    nv     = map toNoteView (toList issues)
    sample = Sample (take limit nv) (genericLength nv)

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
        Just (URL.URL url) -> "\nurl " <> url
        _                  -> ""
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId (Id n) = DocId $ show n

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
