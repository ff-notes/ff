{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Data.Aeson (Value, decode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.String.Interpolate.IsString (i)
import           GHC.Exts (IsList, Item, fromList)
import           Test.Tasty (defaultMain)
import           Test.Tasty.HUnit (testCase, (@?=))

import           FF.Test (TestDB, runStorageSim)
import           FF.Upgrade (upgradeDatabase)

main :: IO ()
main = defaultMain $ testCase "json2ron" $
    case runStorageSim dbIn upgradeDatabase of
        Left e          -> fail e
        Right ((), db') -> db' @?= dbOut

dbIn :: TestDB
dbIn = "note" -:- "1" -:-
    [ "2" -: [i|
        { "status": ["Active", 15154096036380270, 216273105866966]
        , "text":   [[15154096036382360, 216273105866966, "hello"]]
        , "start":  ["2018-02-15", 15186101521538050, 169324965521840]
        , "end":    [null, 0, 0]
        } |]
    , "3" -: [i|
        { "status": ["Active", 15154096036380270, 216273105866966]
        , "text":   [[15154096036382360, 216273105866966, "hello"]]
        , "start":  ["2018-03-31", 15224207489438798, 109952946870224]
        , "end":    [null, 0, 0]
        } |]
    ]

dbOut :: TestDB
dbOut = "note" -:- "1" -:- "a6bp8-6qen" -:- norm [i|
    { "status":     ["Active", 15154096036380270, 216273105866966]
    , "text.trace": ["hello"]
    , "text":       [[15154096036382360, 216273105866966, "hello"]]
    , "start":      ["2018-03-31", 15224207489438798, 109952946870224]
    , "end":        [null, 0, 0]
    } |]

(-:) :: a -> b -> (a, b)
a -: b = (a, b)
infix 0 -:

(-:-) :: (IsList list, Item list ~ (a, b)) => a -> b -> list
a -:- b = fromList [(a, b)]
infixr 0 -:-

norm :: ByteString -> ByteString
norm = encode . decode @Value
