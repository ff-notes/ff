{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database
  ( databaseTests,
  )
where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import GitHub (Issue (..), IssueLabel (..), IssueNumber (IssueNumber),
               IssueState (..), Milestone (..), URL (..))
import GitHub.Data.Definitions (SimpleUser (..))
import GitHub.Data.Id (Id (..))
import GitHub.Data.Name (Name (..))
import Hedgehog (Gen, Property, PropertyT, evalEither, evalMaybe, forAll,
                 property, withTests, (===))
import RON.Data (ReplicatedAsObject (readObject), evalObjectState,
                 newObjectFrame)
import RON.Data.RGA (RGA (RGA))
import RON.Storage.Backend (DocId (DocId))
import RON.Storage.Test (TestDB, runStorageSim)
import RON.Text (parseObject, serializeObject)
import RON.Types (ObjectRef (ObjectRef))
import RON.UUID qualified as UUID
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import FF (cmdNewNote, defaultNoteFilter, loadAllNotes, viewTaskSamples)
import FF.Config (defaultConfigUI)
import FF.Github qualified as Github
import FF.Options (New (..))
import FF.Types (Limit, Note (..), NoteStatus (TaskStatus),
                 Sample (Sample, items, total), Status (Active),
                 TaskMode (Overdue), Track (..), View (NoteView, note, tags),
                 entityVal, pattern Entity)

import Gen qualified

databaseTests :: TestTree
databaseTests = $(testGroupGenerator)

prop_not_exist :: Property
prop_not_exist =
  property1 $ do
    (agenda, fs') <-
      evalEither $
      runStorageSim fs $ do
        notes <- loadAllNotes
        viewTaskSamples
          defaultNoteFilter
          defaultConfigUI
          agendaLimit
          today
          notes
    Map.empty === agenda
    fs === fs'
  where
    fs = Map.empty

prop_smoke :: Property
prop_smoke =
  property1 $ do
    (agenda', fs') <-
      evalEither $
      runStorageSim fs123 $ do
        notes <- loadAllNotes
        viewTaskSamples
          defaultNoteFilter
          defaultConfigUI
          agendaLimit
          today
          notes
    agenda === agenda'
    fs123 === fs'
  where
    agenda =
      Map.singleton
        (Overdue 365478)
        Sample
          { items =
              [ Entity
                  (DocId "700000000002D-200000000002D")
                  NoteView
                    { note =
                        Note
                          { note_status = Just $ TaskStatus Active,
                            note_text = Just $ RGA "helloworld",
                            note_start = Just $ fromGregorian 22 11 24,
                            note_end = Just $ fromGregorian 17 06 19,
                            note_tags = [],
                            note_track = Nothing,
                            note_links = []
                          },
                      tags = mempty
                    }
              ],
            total = 1
          }

fs123 :: TestDB
fs123 =
  Map.singleton "note" $ Map.singleton "700000000002D-200000000002D"
    $ Map.fromList
        [ ( "event 2 93",
            BSLC.lines
              [i|
                *lww #7/000000001D+000000001D !
                  @20+21  :end    >some =17 =06 =19
                  @25+26  :start  =22 =11 =24
                  @29+30  :status >Active
                  @07+07  :text   >2+I9
                          :track  >none
                *rga #2+I9 :0 !
                  @6+7            'h'
                  @7+7            'e'
                  @8+7            'l'
                  @9+7            'l'
                  @A+7            'o'
                |]
          ),
          ( "event 3 99",
            BSLC.lines
              [i|
                *lww #000000001Q$000000001Q !
                  @15+16  :end    >some =12 =01 =14
                  @07+08  :start  =09 =10 =11
                  @27+28  :status >Active
                  @04+05  :text   >2+I9
                          :track  >none
                *rga #2+I9 :0 !
                  @4+5            'w'
                  @5+5            'o'
                  @6+5            'r'
                  @7+5            'l'
                  @8+5            'd'
                |]
          )
        ]

agendaLimit :: Maybe Limit
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: Property
prop_new =
  let text = "Мир"
      start = Just $ fromGregorian 2154 5 6
      end = Just $ fromGregorian 3150 1 2
      tags = Set.fromList ["Список", "тэг"]
      fs =
        Map.unions
          [ Map.singleton "note" $
              Map.singleton "7000000007N4M-2000000000012" $
                Map.singleton "700000000P8JM-2000000000012" $
                  map encodeUtf8 $
                    mconcat
                      [ [ "*set\t#7/0000000ynM+000000000Y\t!",
                          "\t@`{1DnM\t>end 3150 1 2",
                          "\t@}TnM\t>start 2154 5 6",
                          "\t@}inM\t>status >Active",
                          "\t@}ynM\t>tags >7/0000000Drz+000000000Y",
                          "\t@{2DnM\t>tags >7/0000000_QM+000000000Y",
                          "\t@}TnM\t>text >7/0000002inM+000000000Y"
                        ],
                        [ "*rga\t#{2inM\t@0\t!",
                          "\t@`}y_h\t'М'",
                          "\t@)i\t'и'",
                          "\t@)j\t'р'",
                          "."
                        ]
                      ],
            Map.singleton "tag" $
              Map.unions
                [ Map.singleton "7000000001NDU-2000000000012" $
                    Map.singleton "70000000039SM-2000000000012" $
                      map
                        encodeUtf8
                        [ "*set\t#7/0000000Drz+000000000Y\t!",
                          "\t@`}IOM\t>text 'Список'",
                          "."
                        ],
                  Map.singleton "7000000004HKM-2000000000012" $
                    Map.singleton "7000000006N4M-2000000000012" $
                      map
                        encodeUtf8
                        [ "*set\t#7/0000000_QM+000000000Y\t!",
                          "\t@`}mnM\t>text 'тэг'",
                          "."
                        ]
                ]
          ]
   in property1 $ do
        (note, fs') <-
          evalEither $
          runStorageSim mempty $
          cmdNewNote New{text, start, end, isWiki = False, tags} today
        tags' <-
          for
            ["7000000001NDU-2000000000012", "7000000004HKM-2000000000012"]
            (fmap ObjectRef . evalMaybe . UUID.decodeBase32)
        let Note{note_text, note_start, note_end, note_tags} = entityVal note
        Just (RGA $ Text.unpack text) === note_text
        start === note_start
        end === note_end
        tags' === note_tags
        fs === fs'

jsonRoundtrip :: (Eq a, FromJSON a, Show a, ToJSON a) => Gen a -> Property
jsonRoundtrip genA = property $ do
  a <- forAll genA
  a' <- evalEither $ parseEither parseJSON $ toJSON a
  a === a'

ronRoundtrip :: (Eq a, ReplicatedAsObject a, Show a) => Gen a -> Property
ronRoundtrip genA = property $ do
  a <- forAll genA
  (obj, _) <- evalEither $ runStorageSim mempty $ newObjectFrame a
  let (u, bs) = serializeObject obj
  obj' <- evalEither $ parseObject u bs
  obj === obj'
  a' <- evalEither $ evalObjectState obj' readObject
  a === a'

prop_Config_JSON :: Property
prop_Config_JSON = jsonRoundtrip Gen.config

test_RON_Tests :: [TestTree]
test_RON_Tests =
  [ testProperty "Contact" $ ronRoundtrip Gen.contact,
    testProperty "Note" $ ronRoundtrip Gen.note
  ]

prop_issues_imported_from_GitHub_can_be_viewed_as_notes :: Property
prop_issues_imported_from_GitHub_can_be_viewed_as_notes =
  property1 $
    ideal === Github.sampleMap "ff-notes/ff" limit todayForIssues issues
  where
    ideal =
      Map.singleton
        (Overdue 10)
        Sample
          { items =
              [ Entity
                  (DocId "")
                  NoteView
                    { note = Note
                        { note_status = Just $ TaskStatus Active,
                          note_text = Just $ RGA "import issues (GitHub -> ff)",
                          note_start = Just $ fromGregorian 2018 06 21,
                          note_end   = Just $ fromGregorian 2018 06 15,
                          note_tags  = [],
                          note_track = Just Track
                            { track_provider   = Just "github",
                              track_source     = Just "ff-notes/ff",
                              track_externalId = Just "60",
                              track_url        =
                                Just "https://github.com/ff-notes/ff/issues/60"
                            },
                          note_links = []
                        },
                      tags =
                        Map.fromList
                          [ ( "https://api.github.com/repos/ff-notes/ron/labels\
                              \/level_Research"
                            , "level_Research"
                            )
                          , ( "https://api.github.com/repos/ff-notes/ron/labels\
                              \/type_Enhancement"
                            , "type_Enhancement"
                            )
                          ]
                    }
              ],
            total = 1
          }

todayForIssues :: Day
todayForIssues = fromGregorian 2018 06 25

limit :: Maybe Limit
limit = Just 1

issues :: [Issue]
issues =
  pure Issue
    { issueClosedAt = Nothing,
      issueUpdatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60 + 41),
      issueEventsUrl = api "issues/60/events",
      issueHtmlUrl = Just $ URL "https://github.com/ff-notes/ff/issues/60",
      issueClosedBy = Nothing,
      issueLabels = labels,
      issueNumber = IssueNumber 60,
      issueAssignees = mempty,
      issueUser = cblp,
      issueTitle = "import issues (GitHub -> ff)",
      issuePullRequest = Nothing,
      issueUrl = api "issues/60",
      issueCreatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60),
      issueBody = Just "",
      issueState = StateOpen,
      issueId = Id 334520780,
      issueComments = 0,
      issueMilestone = Just Milestone
        { milestoneCreator = cblp,
          milestoneDueOn =
            Just $ UTCTime (fromGregorian 2018 06 15) (7 * 3600),
          milestoneOpenIssues = 5,
          milestoneNumber = Id 1,
          milestoneClosedIssues = 0,
          milestoneDescription = Just "",
          milestoneTitle = "GitHub sync",
          milestoneUrl = api "milestones/1",
          milestoneCreatedAt =
            UTCTime (fromGregorian 2018 06 16) (9 * 3600 + 15 * 60 + 35),
          milestoneState = "open"
        }
    }
  where
    api x = URL $ "https://api.github.com/repos/ff-notes/ff/" <> x
    cblp = SimpleUser
      { simpleUserId = Id 63495,
        simpleUserLogin = N "cblp",
        simpleUserAvatarUrl =
          URL "https://avatars0.githubusercontent.com/u/63495?v=4",
        simpleUserUrl = URL "https://api.github.com/users/cblp"
      }
    labels = Vector.fromList
      [ IssueLabel
          { labelColor = "7057ff"
          , labelUrl = URL "https://api.github.com/repos/ff-notes/ron/labels/level_Research"
          , labelName = "level_Research"
          , labelDesc = Nothing
          }
      , IssueLabel
          { labelColor = "b60205"
          , labelUrl = URL "https://api.github.com/repos/ff-notes/ron/labels/type_Enhancement"
          , labelName = "type_Enhancement"
          , labelDesc = Nothing
          }
      ]

property1 :: PropertyT IO () -> Property
property1 = withTests 1 . property
