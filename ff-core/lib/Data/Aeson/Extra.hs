module Data.Aeson.Extra (module Data.Aeson, module Data.Aeson.Extra) where

import           Data.Aeson

untaggedSum :: Options
untaggedSum = defaultOptions{sumEncoding = UntaggedValue}

singletonObjectSum :: Options
singletonObjectSum = defaultOptions{sumEncoding = ObjectWithSingleField}
