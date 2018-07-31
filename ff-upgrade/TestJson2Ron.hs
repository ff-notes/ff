{-# LANGUAGE MultiWayIf #-}

import           Data.Foldable (for_)
import           System.Directory (copyFile, createDirectory,
                                   doesDirectoryExist, doesFileExist,
                                   listDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)

import           FF.Upgrade (upgradeDatabase)

main :: IO ()
main =
    withSystemTempDirectory "TestJson2Ron" $ \tmp -> do
        copyContent "TestJson2Ron.in" tmp
        upgradeDatabase tmp
        diff "TestJson2Ron.out" tmp

copyContent :: FilePath -> FilePath -> IO ()
copyContent src dst = do
    content <- listDirectory src
    for_ content $ \name -> do
        let src' = src </> name
            dst' = dst </> name
        nameIsDir <- doesDirectoryExist src'
        nameIsFile <- doesFileExist src'
        if  | nameIsDir -> do
                createDirectory dst'
                copyContent src' dst'
            | nameIsFile ->
                copyFile src' dst'
            | otherwise ->
                fail "only plain files and dirs are supported"

diff :: FilePath -> FilePath -> IO ()
diff a b = callProcess "diff" ["--recursive", "--unified", a, b]
