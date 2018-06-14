{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , toDoc
    ) where

import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable (toList)
import           Data.List (genericLength)
import           Data.Text (concat, pack)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           FF.Storage (DocId (..))
import           FF.Types (NoteId, NoteView (..), Sample (..), Status (..),
                           TaskMode (..), singletonSampleMap)
import           FF.UI (samplesInSections)
import           GitHub (Issue (..), IssueState (..), Milestone (..),
                         issueCreatedAt, issueId, issueMilestone, issueState,
                         issueTitle, issueUrl)
import           GitHub.Data.Definitions (Error (..), Owner)
import           GitHub.Data.Id
import           GitHub.Data.Name (Name)
import           GitHub.Data.Options (stateOpen)
import           GitHub.Data.Repos (Repo)
import           GitHub.Data.URL (getUrl)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           GitHub.Internal.Prelude (Vector (..))
import           Text.PrettyPrint.Mainland (Doc, pretty)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

runCmdGithub :: Name Owner -> Name Repo -> Int -> IO (Either Error (Vector Issue))
runCmdGithub owner repo limit = issuesForRepo owner repo stateOpen

toDoc :: Int -> Vector Issue -> Doc
toDoc limit issues = samplesInSections limit $ singletonSampleMap Actual (Sample (take limit nv) (genericLength nv)) where
    nv = map toNoteView (toList issues)

toNoteView :: Issue -> NoteView
toNoteView Issue{..} = NoteView
    { nid    = toNoteId issueId
    , status = toStatus issueState
    , text   = Text.concat [issueTitle, Text.pack "\nurl ", getUrl issueUrl]
    , start  = utctDay issueCreatedAt
    , end    = fmap utctDay (join $ milestoneDueOn <$> issueMilestone)
    }

toNoteId :: Id Issue -> NoteId
toNoteId (Id n) = DocId $ show n

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
