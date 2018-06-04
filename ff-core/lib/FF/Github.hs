{-# LANGUAGE OverloadedStrings #-}

module FF.Github
    ( runCmdGithub
    ) where

import           Data.List (intercalate)
import qualified Data.Vector as Vector
import           GitHub.Data.Issues (issueCreatedAt, issueId, issueTitle,
                                     issueUrl)
import           GitHub.Data.Options (optionsNoMilestone)
import           GitHub.Endpoints.Issues (issuesForRepo)
import           GitHub.Endpoints.Repos (Issue)
import           System.IO (hPrint, stderr)

runCmdGithub :: IO ()
runCmdGithub = do
    possibleIssues <- issuesForRepo "ff-notes" "ff" optionsNoMilestone
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
