{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Regression
  ( mkRegressionTest,
  )
where

import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Traversable (for)
import Data.Yaml (encodeFile)
import FF (toNoteView)
import FF.Types
  ( Entity (Entity, entityVal),
    Link,
    Note,
    NoteId,
    NoteStatus,
    Status,
    Tag,
    Track,
    View (NoteView),
    loadNote,
  )
import RON.Data.ORSet (ORSet)
import RON.Data.RGA (RGA)
import RON.Storage (CollectionName)
import RON.Storage.Backend (getCollections, getDocuments)
import RON.Storage.FS (Handle, newHandle, runStorage)
import RON.Types (ObjectRef, UUID)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

deriveToJSON defaultOptions ''Link

deriveToJSON defaultOptions ''Note

deriveToJSON defaultOptions ''NoteStatus

deriveToJSON defaultOptions ''ObjectRef

deriveToJSON defaultOptions ''ORSet

deriveToJSON defaultOptions ''RGA

deriveToJSON defaultOptions ''Status

deriveToJSON defaultOptions ''Tag

deriveToJSON defaultOptions ''Track

deriveToJSON defaultOptions ''UUID

deriveToJSON defaultOptions 'NoteView

mkRegressionTest :: IO (FilePath -> TestTree)
mkRegressionTest = do
  h <- newHandle "../.ff"
  collections <- runStorage h getCollections
  tests <-
    for collections $ \collection -> case collection of
      "note" -> testNoteCollection h collection
      _ -> fail $ "unsupported type " ++ show collection
  pure $ \tmp -> testGroup "regression" [test tmp | test <- tests]

testNoteCollection :: Handle -> CollectionName -> IO (FilePath -> TestTree)
testNoteCollection h collectionName = do
  docs <- runStorage h $ getDocuments @_ @Note
  pure $ \tmp -> testGroup collectionName $ map (testNote h tmp) docs

testNote :: Handle -> FilePath -> NoteId -> TestTree
testNote h tmp docid =
  goldenVsFileDiff (show docid) diff ("ff.dump" </> show docid) outFile action
  where
    outFile = tmp </> show docid
    action = do
      Entity {entityVal} <- runStorage h $ loadNote docid >>= toNoteView
      createDirectoryIfMissing True $ takeDirectory outFile
      encodeFile outFile entityVal

diff :: String -> String -> [String]
diff ref new = ["colordiff", "-u", ref, new]
