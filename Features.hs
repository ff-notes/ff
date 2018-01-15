{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           Data.String (IsString, fromString)
import           Data.String.Interpolate.IsString (i)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5 (Html, code, li, meta, string, style, table,
                                   td, th, tr, ul, (!))
import           Text.Blaze.Html5.Attributes (charset, class_, colspan, rowspan)

type Implemetation = Bool

data Feature = Feature
    { fDescription  :: String
    , fCli          :: Implemetation
    , fQt           :: Implemetation
    , fGtk          :: Implemetation
    , fWx           :: Implemetation
    , fAndroid      :: Implemetation
    , fIos          :: Implemetation
    }

instance IsString Feature where
    fromString description = Feature
        { fDescription = fromString description
        , fCli = False
        , fQt = False
        , fGtk = False
        , fWx = False
        , fAndroid = False
        , fIos = False
        }

features :: [Feature]
features =
    [ "Agenda" & cli & qt
    , "- Show only active" & cli & qt
    , "- Show only started (now >= start)"
    , "- Show start/end dates"
    , "Add note/task" & cli
    , "- with start date" & cli
    , "- with start time"
    , "- with end date" & cli
    , "- with end time"
    , "Mark as done/archive" & cli
    , "Configuration" & cli
    , "- detect folder" & cli
    , "- - ~/Yandex.Disk" & cli
    , "- - ~/Yandex.Disk.localized" & cli
    , "UI" & cli & qt
    , "- поддержка русского языка (кириллицы, Юникода) в заметках" & cli
    ]

main :: IO ()
main = writeFile "/tmp/features.html" $ renderHtml $ do
    meta ! charset "utf-8"
    style [i|
        table { border-collapse: collapse; }
        th, td { border: solid 1px; }
        td.no  { background: lightcoral; }
        td.yes { background: lightgreen; }
        ul { margin: 0; }
        |]
    table $ do
        tr $ do
            th ! rowspan "2" $ "Feature"
            th ! rowspan "2" $ "CLI" <> code "ff"
            th ! colspan "3" $ "GUI"
            th ! colspan "2" $ "Mobile"
        tr $ do
            th $ code "ff-qt"
            th $ code "ff-gtk"
            th $ code "ff-wx"
            th "Android"
            th "iOS"
        for_ features $ \Feature{..} -> tr $ do
            td $ deep fDescription
            check True  fCli
            check True  fQt
            check False fGtk
            check False fWx
            check True  fAndroid
            check False fIos

check
    :: Bool -- ^ needed
    -> Bool -- ^ implemeted
    -> Html
check False False = td ""
check True  False = td ! class_ "no"  $ "❌"
check _     True  = td ! class_ "yes" $ "✅"

deep :: String -> Html
deep = \case
    '-' : ' ' : s -> ul $ li $ deep s
    s -> string s

cli, qt :: Feature -> Feature
cli f = f{fCli = True}
qt  f = f{fQt  = True}
