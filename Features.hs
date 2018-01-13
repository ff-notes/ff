{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Foldable (for_)
import           Data.Semigroup ((<>))
import           Data.String (IsString, fromString)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5 (Html, code, li, string, style, table, td, th,
                                   tr, ul, (!))
import qualified Text.Blaze.Html5.Attributes as A

type Implemetation = Bool

data Feature = Feature
    { description :: String
    , cli         :: Implemetation
    , qt          :: Implemetation
    , gtk         :: Implemetation
    , wx          :: Implemetation
    , android     :: Implemetation
    , ios         :: Implemetation
    }

instance IsString Feature where
    fromString description = Feature
        { description = fromString description
        , cli = False
        , qt = False
        , gtk = False
        , wx = False
        , android = False
        , ios = False
        }

features :: [Feature]
features =
    [ "Agenda"{cli=True, qt=True}
    , "- Show only active"{cli=True, qt=True}
    , "- Show only started (now >= start)"
    , "- Show start/end dates"
    , "Add note/task"{cli=True}
    , "- with start date"{cli=True}
    , "- with start time"
    , "- with end date"{cli=True}
    , "- with end time"
    , "Mark as done/archive"{cli=True}
    , "Configuration"{cli=True}
    , "- detect folder"{cli=True}
    , "- - ~/Yandex.Disk"{cli=True}
    , "- - ~/Yandex.Disk.localized"{cli=True}
    ]

main :: IO ()
main = writeFile "/tmp/features.html" $ renderHtml $ do
    style "th, td { border: solid 1px; }"
    style "table { border-collapse: collapse; }"
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
            td $ deep description
            td $ check cli
            td $ check qt
            td $ check gtk
            td $ check wx
            td $ check android
            td $ check ios

check :: IsString string => Bool -> string
check False = "❌"
check True = "✅"

deep :: String -> Html
deep = \case
    '-' : ' ' : s -> ul $ li $ deep s
    s -> string s
