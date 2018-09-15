{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types2 where

import qualified Data.Text as Text
import           Data.Time (Day, fromGregorian, toGregorian)
import           RON.Data (Replicated (..), ReplicatedAsPayload (..),
                           payloadEncoding)
import           RON.Schema (AliasAnnotations (..), Declaration (..),
                             RonType (..), StructAnnotations (..),
                             StructLww (..), TAtom (..), alias, def, field,
                             option, rgaString)
import           RON.Schema.TH (mkReplicated)
import           RON.Types (Atom (..))
import           Text.Read (readEither)

data Status = Active | Archived | Deleted
    deriving (Bounded, Enum, Eq, Read, Show)

instance Replicated Status where encoding = payloadEncoding

instance ReplicatedAsPayload Status where
    toPayload = toPayload . Text.pack . show
    fromPayload atoms = readEither =<< fmap Text.unpack (fromPayload atoms)

data NoteStatus = TaskStatus Status | Wiki
    deriving (Eq, Show)

instance Replicated NoteStatus where encoding = payloadEncoding

instance ReplicatedAsPayload NoteStatus where
    toPayload = \case
        TaskStatus status -> toPayload status
        Wiki              -> [AString "Wiki"]
    fromPayload = \case
        [AString "Wiki"] -> pure Wiki
        p                -> TaskStatus <$> fromPayload p

$(let
    day = alias (TAtomTuple [TAInteger, TAInteger, TAInteger])
        def{aaHaskellType = Just "Day"}
    status = alias (TAtom TAString) def{aaHaskellType = Just "Status"}
    noteStatus =
        alias (TAtom TAString) def{aaHaskellType = Just "NoteStatus"}
    tracked = StructLww "Tracked"
        [ ("provider",   field $ TAtom TAString)
        , ("source",     field $ TAtom TAString)
        , ("externalId", field $ TAtom TAString)
        , ("url",        field $ TAtom TAString)
        ]
        def
    contact = StructLww "Contact"
        [("status", field status), ("name", field rgaString)]
        def{saHaskellFieldPrefix = "contact_"}
    note = StructLww "Note"
        [ ("status",  field noteStatus)
        , ("text",    field rgaString)
        , ("start",   field day)
        , ("end",     field $ option day)
        , ("tracked", field $ option $ TStructLww tracked)
        ]
        def{saHaskellFieldPrefix = "note_"}
    in mkReplicated [DStructLww tracked, DStructLww contact, DStructLww note])

-- * Orphans

instance Replicated Day where encoding = payloadEncoding

instance ReplicatedAsPayload Day where
    toPayload day =
        map AInteger [fromIntegral y, fromIntegral m, fromIntegral d]
      where
        (y, m, d) = toGregorian day

    fromPayload = \case
        [AInteger y, AInteger m, AInteger d] -> pure $
            fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
        _ -> Left "bad Day"
