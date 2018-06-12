module FF.Github
    ( runCmdGithub
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable (toList)
import           Data.List (genericLength)
import           Data.Time (Day, UTCTime (..))
import           FF.Storage (DocId (..))
import           FF.Types (NoteId, NoteView (..), Sample (..), Status (..),
                           TaskMode (..), singletonSampleMap)
import           FF.UI (samplesInSections)
import           GitHub (Issue (..), IssueState (..), issueClosedAt,
                         issueCreatedAt, issueId, issueState, issueTitle)
import           GitHub.Data.Definitions (Owner)
import           GitHub.Data.Id
import           GitHub.Data.Name (Name)
import           GitHub.Data.Options (stateOpen)
import           GitHub.Data.Repos (Repo)
import           GitHub.Endpoints.Issues (issuesForRepo)
import qualified System.Console.Terminal.Size as Terminal
import           System.IO (hPrint, stderr)
import           Text.PrettyPrint.Mainland (Doc, pretty)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

runCmdGithub :: Name Owner -> Name Repo -> Int -> IO ()
runCmdGithub owner repo limit = do
    possibleIssues <- issuesForRepo owner repo stateOpen
    case possibleIssues of
        Left err ->
            hPrint stderr err
        Right issues -> do
            let nv = map toNoteView (toList issues)
            pprint $ toDoc limit nv

toDoc :: Int -> [NoteView] -> Doc
toDoc limit nv = samplesInSections limit $ singletonSampleMap Actual (Sample (take limit nv) (genericLength nv)) -- Правильно ли указан TaskMode, а если нет, то как его указать?

toNoteView :: Issue -> NoteView
toNoteView issue = NoteView
    { nid    = toNoteId $ issueId issue
    , status = toStatus $ issueState issue
    , text   = issueTitle issue
    , start  = toDay $ issueCreatedAt issue
    , end    = toMaybeDay $ issueClosedAt issue
    }

toNoteId :: Id Issue -> NoteId
toNoteId (Id n) = DocId $ show n

toStatus :: IssueState -> Status
toStatus is = case is of
    StateOpen   -> Active
    StateClosed -> Archived

toDay :: UTCTime -> Day
toDay = utctDay

toMaybeDay :: Maybe UTCTime -> Maybe Day
toMaybeDay Nothing = Nothing
toMaybeDay (Just utc) = Just $ utctDay utc

pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- maybe 80 Terminal.width <$> Terminal.size
    putStrLn . pretty width $ ppr a
