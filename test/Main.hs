{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Monad.State.Strict (StateT, get, runStateT)
import           CRDT.LamportClock (Clock, Pid (Pid), Process)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)
import           Data.Aeson (Value (Number, String), object, parseJSON, (.=))
import           Data.Aeson.Types (parseEither)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time (Day, fromGregorian)
import           GHC.Exts (fromList)
import           System.FilePath (splitDirectories)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (getSamples)
import           FF.Storage (Collection, DocId (DocId), MonadStorage (..),
                             Version, collectionName)
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

    createFile = undefined

    readFile :: forall doc. Collection doc => DocId doc -> Version -> TestM doc
    readFile (DocId docId) version = TestM $ do
        fs <- get
        go fs [collectionName @doc, docId, version]
      where
        go dir = \case
            []        -> fail "is directory"
            name:rest -> case Map.lookup name dir of
                Nothing             -> fail "does not exist"
                Just (Dir subdir)   -> go subdir rest
                Just (File content) ->
                    either fail pure $ parseEither parseJSON content

runTestM :: Monad m => Dir -> TestM a -> m (a, Dir)
runTestM fs (TestM stateful) =
    either fail pure . runLamportClockSim . runProcessSim (Pid 42) $
        runStateT stateful fs

main :: IO ()
main = $defaultMainGenerator

case_not_exist :: IO ()
case_not_exist = do
    (agenda, fs') <- runTestM fs $ getSamples agendaLimit today
    agenda @?= emptySampleMap
    fs' @?= fs
  where
    fs = Map.empty

case_smoke :: IO ()
case_smoke = do
    (agenda, fs') <- runTestM fs123 $ getSamples agendaLimit today
    agenda @?=
        emptySampleMap
            { overdue = Sample
                { notes =
                    [NoteView
                        { nid = DocId "1"
                        , text = "hello"
                        , start = fromGregorian 22 11 24
                        , end = Just $ fromGregorian 17 06 19
                        }]
                , total = 1
                }
            }
    fs' @?= fs123

fs123 :: Dir
fs123 = Map.singleton "note" $
    Dir $ Map.singleton "1" $
    Dir $ fromList
        [   ( "2"
            , File $ object
                [ "end"    .= [String "17-06-19", Number 20, Number 21]
                , "start"  .= [String "22-11-24", Number 25, Number 26]
                , "status" .= [String "Active",   Number 29, Number 30]
                , "text"   .= [String "hello",    Number  6, Number  7]
                ]
            )
        ,   ( "3"
            , File $ object
                [ "end"    .= [String "12-01-14", Number 15, Number 16]
                , "start"  .= [String  "9-10-11", Number  7, Number  8]
                , "status" .= [String "Active",   Number 27, Number 28]
                , "text"   .= [String "world",    Number  4, Number  5]
                ]
            )
        ]

agendaLimit :: Int
agendaLimit = 10

today :: Day
today = fromGregorian 2018 02 10
