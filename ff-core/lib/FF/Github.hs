{-# LANGUAGE OverloadedStrings #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.List (intercalate)
import           GitHub (Issue, issueCreatedAt, issueId, issueTitle, issueUrl,
                         optionsNoMilestone)
import           GitHub.Data.Definitions (Owner)
import           GitHub.Data.Name (Name)
import           GitHub.Data.Repos (Repo)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           GitHub.Endpoints.Repos (Issue)
import           System.IO (hPrint, stderr)

runCmdGithub :: Name Owner -> Name Repo -> IO ()
runCmdGithub owner repo = do
    possibleIssues <- issuesForRepo owner repo optionsNoMilestone
    case possibleIssues of
        (Left errorGit) ->
            hPrint stderr errorGit
        (Right issues) ->
            putStrLn $ intercalate "\n\n" $ Vector.toList $ Vector.map formatIssue issues

formatIssue :: Issue -> String
formatIssue issue = concat [
    "     * ff: "
    , show (issueTitle issue)
    , "\n       "
    , "start "
    , take 10 (show (issueCreatedAt issue))
    , "\n       "
    , show (issueId issue)
    , "\n       "
    , show (issueUrl issue)
    ]
