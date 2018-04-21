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

import           Control.Error                ((?:))
import           Control.Monad.State.Strict   (StateT, get, modify, runStateT)
import           CRDT.LamportClock            (Clock, LamportTime, Pid (Pid),
                                               Process)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)
import           Data.Aeson                   (FromJSON, ToJSON, Value (Array, Number, Object, String),
                                               object, parseJSON,
                                               toJSON, (.=))
import           Data.Aeson.Types             (Parser, parseEither)
import           Data.Foldable                (toList)
import           Data.HashMap.Strict          ((!))
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time                    (Day, fromGregorian)
import           Data.Typeable                (Proxy(..), Typeable, typeRep)
import           GHC.Exts                     (fromList)
import           System.FilePath              (splitDirectories)
import           Test.QuickCheck              (Arbitrary, Property,
                                               conjoin, counterexample,
                                               property, (===), (==>))
import           Test.QuickCheck.Instances    ()
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase, (@?=))
import           Test.Tasty.QuickCheck        (testProperty)
import           Test.Tasty.TH                (defaultMainGenerator)

import           FF                           (cmdNew, getSamples)
import           FF.Config                    (Config, ConfigUI (..))
import           FF.Options                   (New (..))
import           FF.Storage                   (Collection, DocId (DocId),
                                               MonadStorage (..), Version,
                                               collectionName,
                                               lamportTimeToFileName)
import           FF.Types                     (Note (..), NoteView (..),
                                               Sample (..), Status (Active),
                                               TaskMode (Overdue),
                                               emptySampleMap,
                                               singletonSampleMap)

import           ArbitraryOrphans             ()

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
            Overdue
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

agendaLimit :: Maybe Int
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: Text -> Maybe Day -> Maybe Day -> Property
prop_new newText newStart newEnd =
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

ui :: ConfigUI
ui = ConfigUI {shuffle = False}

pjsonRoundtrip :: forall a . (Show a, Eq a, FromJSON a, ToJSON a) => a -> Property
pjsonRoundtrip j =
    let res = parseEither (parseJSON :: Value -> Parser a) $ toJSON j in
    case res of
        Left l -> counterexample l $ property False
        Right j' -> j === j'

jsonRoundtrip
    :: forall a . (Show a, Eq a, FromJSON a, ToJSON a, Typeable a, Arbitrary a)
    => Proxy a
    -> TestTree
jsonRoundtrip proxy = testProperty msg (pjsonRoundtrip :: a -> Property)
  where
    msg = "JSON roundtrip " <> show typ
    typ = typeRep proxy

test_JSON_Tests :: [TestTree]
test_JSON_Tests =
    [ jsonRoundtrip (Proxy :: Proxy Config)
    , jsonRoundtrip (Proxy :: Proxy Note)]
