import Graphics.QML (
    contextObject,
    defaultEngineConfig,
    fileDocument,
    initialDocument,
    runEngineLoop,
 )

import FF ()

main :: IO ()
main = do
    runEngineLoop
        defaultEngineConfig
            { initialDocument = fileDocument "ApplicationWindow.qml"
            , contextObject = Nothing -- Just $ anyObjRef ctx
            }
