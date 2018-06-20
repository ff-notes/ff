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
    [ singletonSampleMap mode sample
    | (mode, sample) <- takeFromMany limit groups
    ]
  where
    nvs = map toNoteView (toList issues)
    groups = groupSort [(taskMode today nv, nv) | nv <- nvs]
    takeFromMany _ [] = []
    takeFromMany lim (g:gs) = (\(m, ns) -> (m, Sample (take lim ns) (genericLength ns))) g
                            : takeFromMany (if lim <= len then 0 else lim - len) gs
      where
        len = genericLength . snd $ g

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
