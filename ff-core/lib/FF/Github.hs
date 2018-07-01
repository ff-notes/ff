{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    ) where

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
    -> IO (Either Text (ModeMap Sample))
runCmdGithub address mlimit today = do
    address' <- case address of
        Just a -> case splitR a of
                      [x, y] | not (Text.null x) && not (Text.null y) -> pure $ Right a
                             | otherwise -> wrong a
                      _ -> wrong a
        Nothing -> do
            packed <- Text.pack <$> readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            case Text.stripSuffix ".git\n"
                =<< Text.stripPrefix "https://github.com/" packed of
                Nothing -> pure $ Left "Sorry, only github repository expected."
                Just b  -> pure $ Right b
    case address' of
        Left err -> pure $ Left err
        Right ownerepo -> do
            let (owner, repo) = (\[x,y] -> (x,y)) . splitR $ ownerepo
            let fetching = maybe FetchAll (FetchAtLeast . fromIntegral) mlimit
            let issues = issuesForRepoR (mkOwnerName owner) (mkRepoName repo) mempty fetching
            result <- fmap (sampleMaps mlimit today) <$> executeRequest' issues
            case result of
                Left err -> pure $ Left $ Text.pack $ show err
                Right sm -> pure $ Right sm
  where
    splitR = Text.split (=='/')
    wrong a = pure $ Left $ Text.concat
        ["Something is wrong with "
        , a
        ,". Please, check correctness of input. "
        ,"Right format is OWNER/REPO"
        ]

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
