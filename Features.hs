{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           Data.String (IsString, fromString)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5 (Html, code, li, string, style, table, td, th,
                                   tr, ul, (!))
import qualified Text.Blaze.Html5.Attributes as A

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
    ]

main :: IO ()
main = writeFile "/tmp/features.html" $ renderHtml $ do
    style
        "table { border-collapse: collapse; }\n\
        \th, td { border: solid 1px; }\n\
        \ul { margin: 0; }"
    table $ do
        tr $ do
            th ! A.rowspan "2" $ "Feature"
            th ! A.rowspan "2" $ "CLI" <> code "ff"
            th ! A.colspan "3" $ "GUI"
            th ! A.colspan "2" $ "Mobile"
        tr $ do
            th $ code "ff-qt"
            th $ code "ff-gtk"
            th $ code "ff-wx"
            th "Android"
            th "iOS"
        for_ features $ \Feature{..} -> tr $ do
            td $ deep fDescription
            td $ check True  fCli
            td $ check True  fQt
            td $ check False fGtk
            td $ check False fWx
            td $ check True  fAndroid
            td $ check False fIos

check
    :: IsString string
    => Bool -- ^ needed
    -> Bool -- ^ implemeted
    -> string
check False False = ""
check True  False = "❌"
check _     True  = "✅"

deep :: String -> Html
deep = \case
    '-' : ' ' : s -> ul $ li $ deep s
    s -> string s

cli, qt :: Feature -> Feature
cli f = f{fCli = True}
qt  f = f{fQt  = True}
