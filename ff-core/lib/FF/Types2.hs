{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types2 where

import qualified Data.Text as Text
import           RON.Data (Replicated (..), ReplicatedAsPayload (..),
                           payloadEncoding)
import           RON.Data.Time (Day, day)
import           RON.Schema (Declaration (..), OpaqueAnnotations (..),
                             RonType (..), StructAnnotations (..),
                             StructLww (..), TAtom (..), def, field,
                             opaqueAtoms, rgaString)
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

{-
OGDL version from future:

    import Time Day
    opaque Status
    opaque NoteStatus

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
