{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    , checkError
    ) where

import           Data.Foldable (toList)
-- import           Data.List (find)
-- import           Data.List.Split (splitOneOf)
import           Data.Semigroup ((<>))
-- import           Data.String (fromString)
-- import           Data.Text (Text, breakOn, head, last)
import           Data.Maybe (fromJust, isNothing)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           GitHub (Error, FetchCount (..), Id, Issue (..),
                         IssueState (..), Milestone (..), URL (..),
                         executeRequest', issueCreatedAt, issueHtmlUrl, issueId,
                         issueMilestone, issueState, issueTitle, mkOwnerName,
                         mkRepoName, untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

runCmdGithub
    :: Maybe Text.Text
    -> Limit
    -> Day  -- ^ today
    -> IO (Either Error (ModeMap Sample))
runCmdGithub address limit today = do
    address' <- case address of
        Just a -> pure a
        Nothing -> do
            url <- readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            pure $ Text.drop 19 . Text.dropEnd 5 $ Text.pack url
    let [owner, repo] = Text.splitOn "/" address'
    -- let repo = Text.drop 1 . Text.splitOn "/" $ address'
    let fetching = FetchAtLeast $ fromIntegral limit
    let issues = issuesForRepoR (mkOwnerName owner) (mkRepoName repo) mempty fetching
    fmap (sampleMaps limit today) <$> executeRequest' issues

checkError :: Maybe Text.Text -> Either Text.Text (Maybe Text.Text)
checkError text | isNothing text = Right Nothing
                | otherwise = if (/=Just 2) (length <$> (Text.splitOn "/" <$> text))
                      then Left $ Text.concat
                          ["\nSomething is wrong with "
                          , fromJust text
                          ,"\nPlease, check correction of input."
                          ,"\nRight format is --repo=OWNER/REPO\n"
                          ]
                      else Right text

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
