{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    ) where

import           Data.Foldable (toList)
import           Data.List (find)
import           Data.List.Split (splitOneOf)
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Data.Time (Day, UTCTime (..))
import           GitHub (Error, FetchCount (..), Id, Issue (..),
                         IssueState (..), Milestone (..), Name, Owner, Repo,
                         URL (..), executeRequest', issueCreatedAt,
                         issueHtmlUrl, issueId, issueMilestone, issueState,
                         issueTitle, untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

runCmdGithub
    :: Maybe String
    -> Limit
    -> Day  -- ^ today
    -> IO (Either Error (ModeMap Sample))
runCmdGithub address limit today = do
    address' <- case address of
        Just a -> if find (=='/') a == Just '/'
            then pure a
            else do
                putStrLn $ concat ["\nThere is no slash ('/') between OWNER and REPO."
                                  ,"\nPlease, check correction of  input."
                                  ,"\nRight format is --repo=OWNER/REPO\n"
                                  ]
                pure a
        Nothing -> do
            url <- readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            pure $ drop 19 . take (length url - 5) $ url
    let owner = head . splitOneOf "/" $ address'
    let repo = last . splitOneOf "/" $ address'
    let fetching = FetchAtLeast $ fromIntegral limit
    let issues = issuesForRepoR (mkOwner owner) (mkRepo repo) mempty fetching
    fmap (sampleMaps limit today) <$> executeRequest' issues

mkOwner :: String -> Name Owner
mkOwner = fromString

mkRepo :: String -> Name Repo
mkRepo  = fromString

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
