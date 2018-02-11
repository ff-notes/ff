{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Error ((?:))
import           Control.Monad.State.Strict (StateT, get, modify, runStateT)
import           CRDT.LamportClock (Clock, LamportTime, Pid (Pid), Process)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)
import           Data.Aeson (Value (Number, String), object, parseJSON, toJSON,
                             (.=))
import           Data.Aeson.Types (parseEither)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Map.Strict (Map, singleton)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time (Day, fromGregorian)
import           GHC.Exts (fromList)
import           System.FilePath (splitDirectories)
import           Test.QuickCheck (Property, conjoin, counterexample, property,
                                  (===), (==>))
import           Test.QuickCheck.Instances ()
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (cmdNew, getSamples)
import           FF.Options (New (..))
import           FF.Storage (Collection, DocId (DocId), MonadStorage (..),
                             Version, collectionName, lamportTimeToFileName)
import           FF.Types (ModeMap (..), NoteView (..), Sample (..),
                           emptySampleMap)

data DirItem = Dir Dir | File Value
    deriving (Eq, Show)

type Dir = Map FilePath DirItem

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
    either fail pure . runLamportClockSim . runProcessSim (Pid 42) $ runStateT
        stateful
        fs

main :: IO ()
main = $defaultMainGenerator

case_not_exist :: IO ()
case_not_exist = do
    (agenda, fs') <- runTestM fs $ getSamples agendaLimit today
    agenda @?= emptySampleMap
    fs' @?= fs
    where fs = Map.empty

case_smoke :: IO ()
case_smoke = do
    (agenda, fs') <- runTestM fs123 $ getSamples agendaLimit today
    agenda @?= emptySampleMap
        { overdue = Sample
            { notes = [ NoteView
                            { nid   = DocId "1"
                            , text  = "hello"
                            , start = fromGregorian 22 11 24
                            , end   = Just $ fromGregorian 17 06 19
                            }
                      ]
            , total = 1
            }
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

agendaLimit :: Int
agendaLimit = 10

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
            [ nv === NoteView
                { nid   = DocId "5-16"
                , text  = newText
                , start = newStart ?: today
                , end   = newEnd
                }
            , fs' === singleton
                "note"
                ( Dir
                $ singleton "5-16"
                $ Dir
                $ singleton "6-16"
                $ File
                $ object
                      [ "status" .= ["Active", Number 1, Number 42]
                      , "text" .= [String newText, Number 2, Number 42]
                      , "start"
                          .= [toJSON $ newStart ?: today, Number 3, Number 42]
                      , "end" .= [toJSON newEnd, Number 4, Number 42]
                      ]
                )
            ]

expectJust :: Maybe Property -> Property
expectJust = fromMaybe . counterexample "got Nothing" $ property False
