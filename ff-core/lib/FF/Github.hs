{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , toDoc
    ) where

import           Data.Foldable (toList)
import           Data.List (genericLength)
import           Data.Time (UTCTime (..))
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
import           GitHub.Data.URL (getUrl)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           GitHub.Internal.Prelude (Vector, pack, (<>))
import           Text.PrettyPrint.Mainland (Doc)

runCmdGithub :: Name Owner -> Name Repo -> IO (Either Error (Vector Issue))
runCmdGithub owner repo = issuesForRepo owner repo stateOpen

toDoc :: Int -> Vector Issue -> ModeMap Sample
toDoc limit issues = singletonSampleMap Actual samples
    where
        nv = map toNoteView (toList issues)
        samples = Sample (take limit nv) (genericLength nv)

toNoteView :: Issue -> NoteView
toNoteView Issue{..} = NoteView
    { nid    = toNoteId issueId
    , status = toStatus issueState
    , text   = issueTitle <> maybeUrl
    , start  = utctDay issueCreatedAt
    , end    = maybeMilestone
    }
    where
        maybeUrl = case getUrl <$> issueHtmlUrl of
            Just url -> pack "\nurl " <> url
            _        -> pack ""
        maybeMilestone = case issueMilestone of
            Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
            _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId (Id n) = DocId $ show n

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
