{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as MonadFail
import           Data.Aeson (FromJSON, ToJSON,
                             Value (Array, Number, Object, String), decode,
                             encode, object, parseJSON, toJSON, (.=))
import           Data.Aeson.Types (parseEither)
import           Data.Foldable (toList)
import           Data.HashMap.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..), fromGregorian)
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           GitHub (Issue (..), IssueState (..), Milestone (..), URL (..))
import           GitHub.Data.Definitions (SimpleUser (..))
import           GitHub.Data.Id (Id (..))
import           GitHub.Data.Name (Name (..))
import           Hedgehog (Gen, MonadTest, Property, PropertyT, annotateShow,
                           failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (cmdNewNote, getSamples)
import           FF.Config (ConfigUI (..))
import qualified FF.Github as Github
import           FF.Options (New (..))
import           FF.Storage (DocId (DocId))
import           FF.Test (TestDB, runStorageSim)
import           FF.Types (Limit, NoteStatus (..), NoteView (..), Sample (..),
                           Status (Active), TaskMode (Overdue), Tracked (..))
import           FF.Upgrade (upgradeDatabase)

import qualified Gen

main :: IO ()
main = $defaultMainGenerator

prop_not_exist :: Property
prop_not_exist = property $ do
    (agenda, fs') <-
        either fail pure $ runStorageSim fs $ getSamples ui agendaLimit today
    agenda === Map.empty
    fs' === fs
    where fs = Map.empty

prop_smoke :: Property
prop_smoke = property $ do
    (agenda, fs') <-
        either fail pure $ runStorageSim fs123 $ getSamples ui agendaLimit today
    agenda ===
        Map.singleton
            (Overdue 365478)
            Sample
                { docs = pure NoteView
                    { nid     = Just $ DocId "1"
                    , status  = TaskStatus Active
                    , text    = "helloworld"
                    , start   = fromGregorian 22 11 24
                    , end     = Just $ fromGregorian 17 06 19
                    , tracked = Nothing
                    }
                , total = 1
                }
    fs' === fs123

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

prop_new :: Property
prop_new = property $ do
    newText  <-
        forAll $
        Gen.text (Range.linear 0 10000) $ Gen.filter (/= '\0') Gen.unicode
    newStart <- forAll $ Gen.maybe Gen.day
    newEnd   <-
        forAll $
        Gen.filter (\newEnd -> newStart <= newEnd && Just today <= newEnd) $
        Gen.maybe Gen.day
    (nv, fs') <-
        evalEitherS $
        runStorageSim mempty $
        cmdNewNote New{newText, newStart, newEnd, newWiki = False} today
    case nv of
        NoteView{text, start, end} -> do
            text  === newText
            start === (newStart ?: today)
            end   === newEnd
    case fs' of
        (toList -> [toList -> [toList -> [decode -> Just (Object note)]]]) -> do
            note ! "status"
                === array ["Active", Number 17091260, Number 314159]
            case note ! "text" of
                Array (toList ->
                        [Array (toList ->
                            [Number _, Number 314159, String text])])
                        | not $ Text.null newText ->
                    text === newText
                Array (toList -> []) -> "" === newText
                _ -> failure
            let start = note ! "start"
            Array (toList -> [start', Number _, Number 314159]) <- pure start
            start' === toJSON (newStart ?: today)
            let end = note ! "end"
            Array (toList -> [end', Number _, Number 314159]) <- pure end
            end' === toJSON newEnd
        _ -> do
            annotateShow fs'
            failure

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

jsonRoundtrip
    :: forall a . (Show a, Eq a, FromJSON a, ToJSON a) => Gen a -> Property
jsonRoundtrip genA = property $ do
    a <- forAll genA
    a' <- evalEitherS $ parseEither parseJSON $ toJSON a
    a === a'

test_JSON_Tests :: [TestTree]
test_JSON_Tests =
    [ testProperty "Config" $ jsonRoundtrip Gen.config
    -- , testProperty "Note"   $ jsonRoundtrip Gen.note
    ]

ui :: ConfigUI
ui = ConfigUI {shuffle = False}

-- instance Arbitrary NoContainNul where
--     arbitrary = NoContainNul . Text.filter ('\NUL' /=) <$> arbitrary

prop_repo :: Property
prop_repo = property $
    Github.sampleMap "ff-notes/ff" limit todayForIssues issues === ideal
  where
    ideal = Map.singleton
        (Overdue 10)
        Sample
            { docs = pure NoteView
                { nid     = Nothing
                , status  = TaskStatus Active
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

-- TODO(cblp, 2018-10-01) enable back
-- test_CvRDT_Note :: [TestTree]
-- test_CvRDT_Note = cvrdtLaws @Note

prop_json2ron :: Property
prop_json2ron = property $ do

    -- read JSON, merge, write RON
    do  ((), db') <- either fail pure $ runStorageSim fs123 upgradeDatabase
        db' === fs123merged

    -- idempotency
    do  ((), db') <-
            either fail pure $ runStorageSim fs123merged upgradeDatabase
        db' === fs123merged

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

array :: [Value] -> Value
array = Array . Vector.fromList

instance Monad m => MonadFail (PropertyT m) where
    fail = fail  -- MonadFail.fail via Monad.fail
