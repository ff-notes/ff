{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.Foldable (toList)
import           Data.List (genericLength)
import           Data.List.Extra (groupSort)
import           Data.Semigroup ((<>))
import           Data.Time (Day, UTCTime (..))

import           GitHub (Error, Id, Issue (..), IssueState (..), Milestone (..),
                         Name, Owner, Repo, URL (..), issueCreatedAt,
                         issueHtmlUrl, issueId, issueMilestone, issueState,
                         issueTitle, untagId)
import           GitHub.Endpoints.Issues (issuesForRepo)

import           FF.Storage (DocId (..))
import           FF.Types (ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..), singletonSampleMap, taskMode)

runCmdGithub
    :: Name Owner
    -> Name Repo
    -> Int
    -> Day
    -> IO (Either Error (ModeMap Sample))
runCmdGithub owner repo limit today =
    fmap (sampleMaps limit today) <$> issuesForRepo owner repo mempty

sampleMaps :: Foldable t => Int -> Day -> t Issue -> ModeMap Sample
sampleMaps limit today issues = mconcat
  $ (\(mode, notes) -> singletonSampleMap mode (Sample notes total)) <$> groups
  where
    groups = groupSort [x | nv <- take limit nvs, let x = (taskMode today nv, nv)]
    nvs = map toNoteView (toList issues)
    total = genericLength nvs

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
