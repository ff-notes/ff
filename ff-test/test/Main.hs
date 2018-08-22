{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Control.Error ((?:))
import           CRDT.Laws (cvrdtLaws)
import           Data.Aeson (FromJSON, ToJSON,
                             Value (Array, Number, Object, String), decode,
                             encode, object, parseJSON, toJSON, (.=))
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Foldable (toList)
import           Data.HashMap.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.String.Interpolate.IsString (i)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..), fromGregorian)
import           GitHub (Issue (..), IssueState (..), Milestone (..), URL (..))
import           GitHub.Data.Definitions (SimpleUser (..))
import           GitHub.Data.Id (Id (..))
import           GitHub.Data.Name (Name (..))
import           Test.QuickCheck (Arbitrary, Property, arbitrary, conjoin,
                                  counterexample, property, (===), (==>))
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (cmdNew, getSamples)
import           FF.Config (Config, ConfigUI (..))
import qualified FF.Github as Github
import           FF.Options (New (..))
import           FF.Storage (DocId (DocId))
import           FF.Test (TestDB, runStorageSim)
import           FF.Types (Limit, Note (..), NoteView (..), Sample (..),
                           Status (Active), TaskMode (Overdue), Tracked (..))
import           FF.Upgrade (upgradeDatabase)

import           ArbitraryOrphans ()

main :: IO ()
main = $defaultMainGenerator

case_not_exist :: IO ()
case_not_exist = do
    (agenda, fs') <-
        either fail pure $ runStorageSim fs $ getSamples ui agendaLimit today
    agenda @?= Map.empty
    fs' @?= fs
    where fs = Map.empty

case_smoke :: IO ()
case_smoke = do
    (agenda, fs') <-
        either fail pure $ runStorageSim fs123 $ getSamples ui agendaLimit today
    agenda @?=
        Map.singleton
            (Overdue 365478)
            Sample
                { notes = pure NoteView
                    { nid     = Just $ DocId "1"
                    , status  = Active
                    , text    = "helloworld"
                    , start   = fromGregorian 22 11 24
                    , end     = Just $ fromGregorian 17 06 19
                    , tracked = Nothing
                    }
                , total = 1
                }
    fs' @?= fs123

fs123 :: TestDB
fs123 = Map.singleton "note" $ Map.singleton "1" $ Map.fromList
    [ "2" -: encode $ object
        [ "end"    .= ["17-06-19", Number 20, Number 21]
        , "start"  .= ["22-11-24", Number 25, Number 26]
        , "status" .= ["Active",   Number 29, Number 30]
        , "text"   .= ["hello",    Number  6, Number  7]
        ]
    , "3" -: encode $ object
        [ "end"    .= ["12-01-14", Number 15, Number 16]
        , "start"  .= ["9-10-11",  Number  7, Number  8]
        , "status" .= ["Active",   Number 27, Number 28]
        , "text"   .= ["world",    Number  4, Number  5]
        ]
    ]

agendaLimit :: Maybe Limit
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: NoContainNul -> Maybe Day -> Maybe Day -> Bool -> Property
prop_new (NoContainNul newText) newStart newEnd _ =
    newStart <= newEnd && Just today <= newEnd ==> expectRight test
  where
    test = do
        (nv, fs') <-
            runStorageSim mempty $ cmdNew New{newText, newStart, newEnd, newWiki = False} today
        pure $ conjoin
            [ case nv of
                NoteView{text, start, end} ->
                    conjoin
                        [ text  === newText
                        , start === (newStart ?: today)
                        , end   === newEnd
                        ]
            , case fs' of
                (toList -> [toList -> [toList -> [decode -> Just (Object note)]]]) -> conjoin
                    [ case note ! "status" of
                        Array (toList -> ["Active", Number _, Number 314159])
                            -> ok
                        status -> failProp $ "status = " ++ show status
                    , case note ! "text" of
                        Array (toList ->
                                [Array (toList ->
                                    [Number _, Number 314159, String text])])
                            | not $ Text.null newText
                            -> text === newText
                        Array (toList -> []) | Text.null newText -> ok
                        text -> failProp $ "text = " ++ show text
                    , case note ! "start" of
                        Array (toList -> [start, Number _, Number 314159]) ->
                            start === toJSON (newStart ?: today)
                        start -> failProp $ "start = " ++ show start
                    , case note ! "end" of
                        Array (toList -> [end, Number _, Number 314159]) ->
                            end === toJSON newEnd
                        end -> failProp $ "end = " ++ show end
                    ]
                _ -> failProp $ "expected singleton dir " ++ show fs'
            ]

-- expectJust :: Maybe Property -> Property
-- expectJust = fromMaybe . counterexample "got Nothing" $ property False

expectRight :: Either String Property -> Property
expectRight = either failProp id

failProp :: String -> Property
failProp s = counterexample s $ property False

ok :: Property
ok = property ()

jsonRoundtrip
    :: forall a . (Show a, Eq a, FromJSON a, ToJSON a) => a -> Property
jsonRoundtrip j =
    let res = parseEither (parseJSON :: Value -> Parser a) $ toJSON j in
    case res of
        Left l -> counterexample l $ property False
        Right j' -> j === j'

test_JSON_Tests :: [TestTree]
test_JSON_Tests =
    [ testProperty "Config" $ jsonRoundtrip @Config
    , testProperty "Note"   $ jsonRoundtrip @Note
    ]

ui :: ConfigUI
ui = ConfigUI {shuffle = False}

newtype NoContainNul = NoContainNul Text
    deriving Show

instance Arbitrary NoContainNul where
    arbitrary = NoContainNul . Text.filter ('\NUL' /=) <$> arbitrary

case_repo :: IO ()
case_repo =
    Github.sampleMap "ff-notes/ff" limit todayForIssues issues @?= ideal
  where
    ideal = Map.singleton
        (Overdue 10)
        Sample
            { notes = pure NoteView
                { nid     = Nothing
                , status  = Active
                , text    = "import issues (GitHub -> ff)"
                , start   = fromGregorian 2018 06 21
                , end     = Just $ fromGregorian 2018 06 15
                , tracked = Just Tracked
                    { trackedProvider = "github"
                    , trackedSource = "ff-notes/ff"
                    , trackedExternalId = "60"
                    , trackedUrl = "https://github.com/ff-notes/ff/issues/60"
                    }
                }
            , total = 1
            }

todayForIssues :: Day
todayForIssues = fromGregorian 2018 06 25

limit :: Maybe Limit
limit = Just 1

issues :: [Issue]
issues = pure Issue
    { issueClosedAt = Nothing
    , issueUpdatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60 + 41)
    , issueEventsUrl = api "issues/60/events"
    , issueHtmlUrl = Just $ URL "https://github.com/ff-notes/ff/issues/60"
    , issueClosedBy = Nothing
    , issueLabels = mempty
    , issueNumber = 60
    , issueAssignees = mempty
    , issueUser = cblp
    , issueTitle = "import issues (GitHub -> ff)"
    , issuePullRequest = Nothing
    , issueUrl = api "issues/60"
    , issueCreatedAt = UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60)
    , issueBody = Just ""
    , issueState = StateOpen
    , issueId = Id 334520780
    , issueComments = 0
    , issueMilestone = Just Milestone
        { milestoneCreator = cblp
        , milestoneDueOn = Just $ UTCTime (fromGregorian 2018 06 15) (7 * 3600)
        , milestoneOpenIssues = 5
        , milestoneNumber = Id 1
        , milestoneClosedIssues = 0
        , milestoneDescription = Just ""
        , milestoneTitle = "GitHub sync"
        , milestoneUrl = api "milestones/1"
        , milestoneCreatedAt =
            UTCTime (fromGregorian 2018 06 16) (9 * 3600 + 15 * 60 + 35)
        , milestoneState = "open"
        }
    }
  where
    api x = URL $ "https://api.github.com/repos/ff-notes/ff/" <> x
    cblp = SimpleUser
        { simpleUserId = Id 63495
        , simpleUserLogin = N "cblp"
        , simpleUserAvatarUrl =
            URL "https://avatars0.githubusercontent.com/u/63495?v=4"
        , simpleUserUrl = URL "https://api.github.com/users/cblp"
        }

test_CvRDT_Note :: [TestTree]
test_CvRDT_Note = cvrdtLaws @Note

case_json2ron :: IO ()
case_json2ron = do

    -- read JSON, merge, write RON
    do  ((), db') <- either fail pure $ runStorageSim fs123 upgradeDatabase
        db' @?= fs123merged

    -- idempotency
    do  ((), db') <-
            either fail pure $ runStorageSim fs123merged upgradeDatabase
        db' @?= fs123merged

  where
    fs123merged = Map.singleton "note" $ Map.singleton "1" $
        Map.singleton "a6bp8-6qen" $ norm [i|
            { "status":     ["Active", 29, 30]
            , "text.trace": ["helloworld"]
            , "text":       [[6, 7, "hello"], [4, 5, "world"]]
            , "start":      ["0022-11-24", 25, 26]
            , "end":        ["0017-06-19", 20, 21]
            } |]
    norm = encode . decode @Value

(-:) :: a -> b -> (a, b)
a -: b = (a, b)
infixr 0 -:
