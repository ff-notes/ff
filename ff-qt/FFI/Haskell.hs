module FFI.Haskell where

import           Control.Monad (void)
import           Foreign.C (CString, peekCAString)
import           Foreign.StablePtr (StablePtr, deRefStablePtr)
import           RON.Storage.IO (DocId (DocId), runStorage)
import qualified RON.Storage.IO as Storage

import           FF (cmdDone, cmdPostpone)

{-# ANN module "HLint: ignore Use camelCase" #-}

foreign export ccall c_done :: StablePtr Storage.Handle -> CString -> IO ()
c_done :: StablePtr Storage.Handle -> CString -> IO ()
c_done storagePtr noteIdStr = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    void $ runStorage storageHandle $ cmdDone $ DocId noteId

foreign export ccall c_postpone :: StablePtr Storage.Handle -> CString -> IO ()
c_postpone :: StablePtr Storage.Handle -> CString -> IO ()
c_postpone storagePtr noteIdStr = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    void $ runStorage storageHandle $ cmdPostpone $ DocId noteId
