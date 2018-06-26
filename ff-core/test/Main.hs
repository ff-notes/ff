{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Control.Error ((?:))
import           Control.Monad.State.Strict (StateT, get, modify, runStateT)
import           CRDT.LamportClock (Clock, LamportTime, Pid (Pid), Process)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)
import           Data.Aeson (FromJSON, ToJSON,
                             Value (Array, Number, Object, String), object,
                             parseJSON, toJSON, (.=))
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Foldable (toList)
import           Data.HashMap.Strict ((!))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..), fromGregorian)
import           GHC.Exts (fromList)
import           GitHub (Issue (..), IssueState (..), Milestone (..), URL (..))
import           GitHub.Data.Definitions (SimpleUser (..))
import           GitHub.Data.Id (Id (..))
import           GitHub.Data.Name (Name (..))
import           System.FilePath (splitDirectories)
import           Test.QuickCheck (Arbitrary, Property, arbitrary, conjoin,
                                  counterexample, property, (===), (==>))
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (cmdNew, getSamples)
import           FF.Config (Config, ConfigUI (..))
import           FF.Github (sampleMaps)
import           FF.Options (New (..))
import           FF.Storage (Collection, DocId (DocId), MonadStorage (..),
                             Version, collectionName, lamportTimeToFileName)
import           FF.Types (Limit, Note (..), NoteView (..), Sample (..),
                           Status (Active), TaskMode (Overdue), emptySampleMap,
                           singletonSampleMap)

import           ArbitraryOrphans ()

data DirItem = Dir Dir | File Value
    deriving (Eq, Show)

type Dir = Map FilePath DirItem

data NDirItem = NDir NDir | NFile Value
    deriving (Eq, Show)

type NDir = [NDirItem]

stripNames :: Dir -> NDir
stripNames = map stripNames' . Map.elems

stripNames' :: DirItem -> NDirItem
stripNames' = \case
    Dir  dir  -> NDir $ stripNames dir
    File file -> NFile file

newtype TestM a = TestM (StateT Dir ProcessSim a)
    deriving (Applicative, Clock, Functor, Monad, Process)

instance MonadStorage TestM where
    listDirectoryIfExists relpath = TestM $ do
        fs <- get
        pure . go fs $ splitDirectories relpath
      where
        go dir = \case
            []        -> Map.keys dir
            name:rest -> case Map.lookup name dir of
                Just (Dir subdir) -> go subdir rest
                _                 -> []

    createFile
        :: forall doc.
        Collection doc => DocId doc -> LamportTime -> doc -> TestM ()
    createFile (DocId docId) time doc =
        TestM $ modify $ go $ collectionName @doc :| [docId, version]
      where
        version = lamportTimeToFileName time
        doc' = File $ toJSON doc
        go path base = case path of
            file :| [] -> case Map.lookup file base of
                Nothing -> Map.insert file doc' base
                Just File{} -> error "file already exists"
                Just Dir{} -> error "directory already exists under this name"
            subdir :| (name2:rest) -> case Map.lookup subdir base of
                Nothing -> Map.insert subdir (make $ name2 :| rest) base
                Just File{} -> error "file already exists under this name"
                Just (Dir dir) ->
                    Map.insert subdir (Dir $ go (name2 :| rest) dir) base
        make = \case
            file :| [] -> Dir $ Map.singleton file doc'
            subdir :| (name2:rest) ->
                Dir $ Map.singleton subdir $ make (name2 :| rest)

    readFile :: forall doc. Collection doc => DocId doc -> Version -> TestM doc
    readFile (DocId docId) version = TestM $ do
        fs <- get
        go fs [collectionName @doc, docId, version]
      where
        decode = either fail pure . parseEither parseJSON
        go dir = \case
            []        -> fail "is directory"
            name:rest -> case Map.lookup name dir of
                Nothing             -> fail "does not exist"
                Just (Dir subdir)   -> go subdir rest
                Just (File content) -> decode content

runTestM :: Monad m => Dir -> TestM a -> m (a, Dir)
runTestM fs (TestM stateful) =
    either fail pure
        . runLamportClockSim
        . runProcessSim (Pid 314159)
        $ runStateT stateful fs

main :: IO ()
main = $defaultMainGenerator

case_not_exist :: IO ()
case_not_exist = do
    (agenda, fs') <- runTestM fs $ getSamples ui agendaLimit today
    agenda @?= emptySampleMap
    fs' @?= fs
    where fs = Map.empty

case_smoke :: IO ()
case_smoke = do
    (agenda, fs') <- runTestM fs123 $ getSamples ui agendaLimit today
    agenda @?=
        singletonSampleMap
            (Overdue 365478)
            Sample
                { notes = [ NoteView
                                { nid    = DocId "1"
                                , status = Active
                                , text   = "helloworld"
                                , start  = fromGregorian 22 11 24
                                , end    = Just $ fromGregorian 17 06 19
                                }
                          ]
                , total = 1
                }
    fs' @?= fs123

fs123 :: Dir
fs123 = Map.singleton "note" $ Dir $ Map.singleton "1" $ Dir $ fromList
    [ ( "2"
      , File $ object
          [ "end" .= ["17-06-19", Number 20, Number 21]
          , "start" .= ["22-11-24", Number 25, Number 26]
          , "status" .= ["Active", Number 29, Number 30]
          , "text" .= ["hello", Number 6, Number 7]
          ]
      )
    , ( "3"
      , File $ object
          [ "end" .= ["12-01-14", Number 15, Number 16]
          , "start" .= ["9-10-11", Number 7, Number 8]
          , "status" .= ["Active", Number 27, Number 28]
          , "text" .= ["world", Number 4, Number 5]
          ]
      )
    ]

agendaLimit :: Maybe Limit
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: NoNul -> Maybe Day -> Maybe Day -> Property
prop_new (NoNul newText) newStart newEnd =
    newStart <= newEnd && Just today <= newEnd ==> expectJust test
  where
    test = do
        (nv, fs') <- runTestM Map.empty
            $ cmdNew New {newText , newStart , newEnd } today
        pure $ conjoin
            [ case nv of
                NoteView { text, start, end } ->
                    conjoin
                        [ text === newText
                        , start === (newStart ?: today)
                        , end === newEnd
                        ]
            , case stripNames fs' of
                [NDir [NDir [NFile (Object note)]]] -> conjoin
                    [ case note ! "status" of
                        Array (toList -> ["Active", Number _, Number 314159])
                            -> ok
                        status -> failProp $ "status = " ++ show status
                    , case note ! "text" of
                        Array (toList -> [Array (toList -> [Number _, Number 314159, String text])])
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

expectJust :: Maybe Property -> Property
expectJust = fromMaybe . counterexample "got Nothing" $ property False

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

newtype NoNul = NoNul Text
    deriving Show

instance Arbitrary NoNul where
    arbitrary = NoNul . Text.filter ('\NUL' /=) <$> arbitrary

case_repo :: IO ()
case_repo = do
    let output = sampleMaps limit today issues
    output @?= ideal
      where
        ideal = fromList
            [ ( Overdue 10
              , Sample  { notes = [NoteView { nid = DocId "334520780"
                                            , status = Active
                                            , text = "import issues (GitHub -> ff)\nurl            https://github.com/ff-notes/ff/issues/60"
                                            , start = fromGregorian 2018 06 21
                                            , end = Just (fromGregorian 2018 06 15)}]
                        , total = 1})]

todayForIssues :: Day
todayForIssues = fromGregorian 2018 06 25

limit :: Limit
limit = 1

-- issues :: Issue
issues = [Issue
  { issueClosedAt = Nothing
  , issueUpdatedAt = UTCTime (fromGregorian 2018 06 21) (14*3600+30*60+41)
  , issueEventsUrl = URL "https://api.github.com/repos/ff-notes/ff/issues/60/events"
  , issueHtmlUrl = Just (URL "https://github.com/ff-notes/ff/issues/60")
  , issueClosedBy = Nothing
  , issueLabels = mempty
  , issueNumber = 60
  , issueAssignees = mempty
  , issueUser = SimpleUser  { simpleUserId = Id 63495
                            , simpleUserLogin = N "cblp"
                            , simpleUserAvatarUrl = URL "https://avatars0.githubusercontent.com/u/63495?v=4"
                            , simpleUserUrl = URL "https://api.github.com/users/cblp"
                            }
  , issueTitle = "import issues (GitHub -> ff)"
  , issuePullRequest = Nothing
  , issueUrl = URL "https://api.github.com/repos/ff-notes/ff/issues/60"
  , issueCreatedAt = UTCTime (fromGregorian 2018 06 21) (14*3600+30*60)
  , issueBody = Just ""
  , issueState = StateOpen
  , issueId = Id 334520780
  , issueComments = 0
  , issueMilestone = Just Milestone
      { milestoneCreator = SimpleUser
          { simpleUserId =Id 63495
          , simpleUserLogin = N "cblp"
          , simpleUserAvatarUrl = URL "https://avatars0.githubusercontent.com/u/63495?v=4"
          , simpleUserUrl = URL "https://api.github.com/users/cblp"
          }
      , milestoneDueOn = Just (UTCTime (fromGregorian 2018 06 15) (7*3600))
      , milestoneOpenIssues = 5
      , milestoneNumber = Id 1
      , milestoneClosedIssues = 0
      , milestoneDescription = Just ""
      , milestoneTitle = "GitHub sync"
      , milestoneUrl = URL "https://api.github.com/repos/ff-notes/ff/milestones/1"
      , milestoneCreatedAt = UTCTime (fromGregorian 2018 06 16) (9*3600+15*60+35)
      , milestoneState = "open"
      }
  }]
