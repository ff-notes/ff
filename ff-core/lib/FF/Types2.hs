{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types2 where

import           Data.Maybe (fromJust)
import           RON.Data (Replicated (..), ReplicatedAsPayload (..),
                           payloadEncoding)
import           RON.Data.Time (Day, day)
import           RON.Schema (Declaration (..), OpaqueAnnotations (..),
                             RonType (..), StructAnnotations (..),
                             StructLww (..), TAtom (..), def, field,
                             opaqueAtoms, rgaString)
import           RON.Schema.TH (mkReplicated)
import           RON.Types (Atom (..), UUID)
import qualified RON.UUID as UUID

data Status = Active | Archived | Deleted
    deriving (Bounded, Enum, Eq, Read, Show)

active, archived, deleted :: UUID
active   = fromJust $ UUID.mkName "Active"
archived = fromJust $ UUID.mkName "Archived"
deleted  = fromJust $ UUID.mkName "Deleted"

instance Replicated Status where encoding = payloadEncoding

instance ReplicatedAsPayload Status where
    toPayload = toPayload . \case
        Active   -> active
        Archived -> archived
        Deleted  -> deleted

    fromPayload = \case
        [AUuid u]
            | u == active   -> pure Active
            | u == archived -> pure Archived
            | u == deleted  -> pure Deleted
        _ -> fail "Expected single UUID"

data NoteStatus = TaskStatus Status | Wiki
    deriving (Eq, Show)

wiki :: UUID
wiki = fromJust $ UUID.mkName "Wiki"

instance Replicated NoteStatus where encoding = payloadEncoding

instance ReplicatedAsPayload NoteStatus where
    toPayload = \case
        TaskStatus status -> toPayload status
        Wiki              -> toPayload wiki
    fromPayload = \case
        [AUuid u] | u == wiki -> pure Wiki
        p                     -> TaskStatus <$> fromPayload p

{-
OGDL version from future:

    import Time Day
    opaque Status  -- TODO enum
    opaque NoteStatus  -- TODO transparent enum?

    struct_lww Tracked
        fields
            provider    String
            source      String
            externalId  String
            url         String

    struct_lww Contact
        fields
            status  Status
            name    RgaString
        Haskell field_prefix contact_

    struct_lww Note
        fields
            status  NoteStatus
            text    RgaString
            start   Day
            end     Option Day
            tracked Option Tracked
        Haskell field_prefix note_
-}

$(let
    status = opaqueAtoms def{oaHaskellType = Just "Status"}
    noteStatus = opaqueAtoms def{oaHaskellType = Just "NoteStatus"}
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
        , ("end",     field $ TOption day)
        , ("tracked", field $ TOption $ TStructLww tracked)
        ]
        def{saHaskellFieldPrefix = "note_"}
    in mkReplicated [DStructLww tracked, DStructLww contact, DStructLww note])
