{-# LANGUAGE OverloadedStrings #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.Foldable (toList)
import           Data.List (intercalate)
import           GitHub (Issue, issueCreatedAt, issueId, issueTitle, issueUrl,
                         optionsNoMilestone)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           System.IO (hPrint, stderr)

runCmdGithub :: IO ()
runCmdGithub = do
    possibleIssues <- issuesForRepo "ff-notes" "ff" optionsNoMilestone
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
