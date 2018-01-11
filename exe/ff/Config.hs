{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Data.Aeson.TH (defaultOptions, deriveJSON)

newtype Config = Config
    { dataDir :: Maybe FilePath
    }

emptyConfig :: Config
emptyConfig = Config{dataDir = Nothing}

deriveJSON defaultOptions ''Config
