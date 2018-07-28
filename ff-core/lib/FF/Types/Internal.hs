module FF.Types.Internal where

import           Data.Aeson (camelTo2)
import           Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier,
                                omitNothingFields)

noteJsonOptions :: Options
noteJsonOptions = defaultOptions
    {fieldLabelModifier = camelTo2 '_' . drop 4, omitNothingFields = True}
