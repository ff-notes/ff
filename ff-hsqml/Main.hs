{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Graphics.QML (
    anyObjRef,
    contextObject,
    defPropertyConst',
    defaultEngineConfig,
    fileDocument,
    initialDocument,
    newClass,
    newObject,
    runEngineLoop,
 )

import FF ()

main :: IO ()
main = do
    cls <-
        newClass [defPropertyConst' "ctx_text" \_ -> pure ("my text" :: Text)]
    ctx <- newObject cls ()
    runEngineLoop
        defaultEngineConfig
            { initialDocument = fileDocument "ApplicationWindow.qml"
            , contextObject = Just $ anyObjRef ctx
            }
