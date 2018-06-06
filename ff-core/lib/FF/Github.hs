{-# LANGUAGE OverloadedStrings #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.Foldable (toList)
import           Data.List (intercalate)
import           GitHub (Issue, issueCreatedAt, issueId, issueTitle, issueUrl,
                         optionsNoMilestone)
import           GitHub.Data.Definitions (Owner)
import           GitHub.Data.Name (Name)
import           GitHub.Data.Repos (Repo)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           System.IO (hPrint, stderr)

runCmdGithub :: Name Owner -> Name Repo -> IO ()
runCmdGithub own rep = do
-- runCmdGithub = do
    possibleIssues <- issuesForRepo own rep optionsNoMilestone
    case possibleIssues of
        Left err ->
            hPrint stderr err
        Right issues ->
            putStrLn $ intercalate "\n\n" $ map formatIssue $ toList issues

formatIssue :: Issue -> String
formatIssue issue = concat
    [ "     * ff: "
    , show (issueTitle issue)
    , "\n       "
    , "start "
    , take 10 (show (issueCreatedAt issue))
    , "\n       "
    , show (issueId issue)
    , "\n       "
    , show (issueUrl issue)
    ]
