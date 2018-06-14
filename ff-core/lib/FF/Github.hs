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

import           FF.Storage (DocId (..))
import           FF.Types (ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..), TaskMode (..), singletonSampleMap)

import           GitHub (Error, Id, Issue (..), IssueState (..), Milestone (..),
                         Name, Owner, Repo, URL (..), issueCreatedAt,
                         issueHtmlUrl, issueId, issueMilestone, issueState,
                         issueTitle, stateOpen, untagId)
import           GitHub.Endpoints.Issues (issuesForRepo)

runCmdGithub :: Name Owner -> Name Repo -> Int -> IO (Either Error (ModeMap Sample))
runCmdGithub owner repo limit =
    fmap sampleMap <$> issuesForRepo owner repo stateOpen
  where
    sampleMap issues = singletonSampleMap Actual sample
      where
        sample = Sample (take limit nv) (genericLength nv)
        nv = map toNoteView (toList issues)


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
