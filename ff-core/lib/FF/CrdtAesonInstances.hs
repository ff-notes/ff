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
import           Data.Aeson (FromJSON, ToJSON, Value (Array, Null, String),
                             parseJSON, toJSON, withArray)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Empty (empty, isEmpty)
import           Data.Foldable (toList)
import qualified Data.Text as Text
import           GHC.Exts (fromList)

deriveJSON defaultOptions ''Pid

instance FromJSON a => FromJSON (LWW a) where
    parseJSON = withArray "LWW" $ \a -> case toList a of
        [valueJ, timeJ, pidJ] ->
            LWW <$> parseJSON valueJ <*> parseLamportTime timeJ pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time pid} =
        array [toJSON value, toJSON time, toJSON pid]

instance FromJSON RgaString where
    parseJSON = rgaParseJson

instance ToJSON RgaString where
    toJSON = rgaToJson

rgaParseJson :: Value -> Parser RgaString
rgaParseJson = withArray "RGA"
    $ \a -> fmap RGA.unpack . parseSegments $ toList a
  where

    parseSegments = \case
        [] -> pure []
        arrays@(Array _:_) ->
            traverse (withList "RGA Segment" parseSegment) arrays
        value:timeJ:pidJ:rest -> do -- legacy < 0.3
            vid   <- parseLamportTime timeJ pidJ
            chars <- parseJSON value
            rest' <- parseSegments rest
            pure $ (vid, chars) : rest'
        value:_ -> typeMismatch "Array" value

    parseSegment = \case
        timeJ:pidJ:value -> do
            mchars <- case value of
                [String str]   -> pure $ Text.unpack str
                [Null, countJ] -> do -- keep Null for compatibility with future non-character RGA
                    count <- parseJSON countJ
                    pure $ replicate count empty
                []  -> fail "expected String or Null followed by Number, got []"
                v:_ -> typeMismatch "String or Null followed by Number" v
            time <- parseJSON timeJ
            pid  <- parseJSON pidJ
            let vid = LamportTime time pid
            pure (vid, mchars)
        _ -> fail "expected Array of 3 elements"

withList :: String -> ([Value] -> Parser a) -> Value -> Parser a
withList name p = withArray name (p . toList)

rgaToJson :: RgaString -> Value
rgaToJson rga = array . map segmentToJson $ RGA.pack rga
  where
    segmentToJson (LamportTime time pid, chars) =
        array $ toJSON time : toJSON pid : if all isEmpty chars
            then [Null, toJSON $ length chars]
            else [toJSON chars]

parseLamportTime :: Value -> Value -> Parser LamportTime
parseLamportTime time pid = LamportTime <$> parseJSON time <*> parseJSON pid

array :: [Value] -> Value
array = Array . fromList
