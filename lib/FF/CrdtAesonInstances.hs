{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FF.CrdtAesonInstances
    (
    ) where

import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (LamportTime (LamportTime), Pid)
import           CRDT.LWW (LWW (LWW), time, value)
import           Data.Aeson (FromJSON, ToJSON, Value (Array), parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Foldable (toList)
import           GHC.Exts (fromList)

deriveJSON defaultOptions ''Pid

instance FromJSON a => FromJSON (LWW a) where
    parseJSON (Array a) = case toList a of
        [valueJ, timeJ, pidJ] ->
            LWW <$> parseJSON valueJ <*> parseLamportTime timeJ pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]
    parseJSON v = typeMismatch "Array" v

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time pid} =
        Array $ fromList [toJSON value, toJSON time, toJSON pid]

instance FromJSON RgaString where
    parseJSON (Array a) = fmap RGA.unpack . go $ toList a
      where
        go = \case
            [] -> pure []
            valueJ : timeJ : pidJ : rest -> do
                vid <- parseLamportTime timeJ pidJ
                chars <- parseJSON valueJ
                rest' <- go rest
                pure $ (vid, unpackChars chars) : rest'
            _ -> fail $ unwords
                ["expected array of 3*n values, got", show $ length a, "values"]
        unpackChars = map $ \case
            '\0' -> Nothing
            c    -> Just c
    parseJSON v = typeMismatch "Array" v

instance ToJSON RgaString where
    toJSON rga = Array $ fromList $ do
        (LamportTime time pid, mchars) <- RGA.pack rga
        [toJSON $ packChars mchars, toJSON time, toJSON pid]
      where
        packChars = map $ \case
            Nothing -> '\0'
            Just c  -> c

parseLamportTime :: Value -> Value -> Parser LamportTime
parseLamportTime time pid = LamportTime <$> parseJSON time <*> parseJSON pid
