{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Process where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified GHC.IO.Handle as H
import System.Process hiding (createProcess)
import Control.Concurrent.MVar

import Reflex
import Reflex.Process
import Decoding
import Messages
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Data.Aeson

convert :: FromClientMessage -> ByteString
convert f = handleClientMessage h h h f
  where
    h m = BSL.toStrict (addHeader $ encode m)

updateReqMap :: MVar RequestMap -> FromClientMessage -> IO ()
updateReqMap rvar = handleClientMessage request ignore ignore
  where
    request :: RequestMessage ClientMethod a b -> IO ()
    request (RequestMessage _ i m _)
      = modifyMVar_ rvar (return . updateRequestMap i m)
    ignore _ = return ()


createLSPProcess
  :: (MonadIO m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => CreateProcess
  -> ProcessConfig t FromClientMessage
  -> m (Process t FromServerMessage)
createLSPProcess c p = do
  reqVar <- liftIO (newMVar newRequestMap)
  createRedirectedProcess (input reqVar) (output reqVar) err c p
  where
    input reqVar h = do
      H.hSetBuffering h H.NoBuffering
      let go e = do
            open <- H.hIsOpen h
            when open $ do
              writable <- H.hIsWritable h
              updateReqMap reqVar e
              when writable $ Char8.hPut h (convert e)
      return go

    output reqVar h trigger = do
      H.hSetBuffering h H.LineBuffering
      let go = do
            open <- H.hIsOpen h
            readable <- H.hIsReadable h
            when (open && readable) $ do
              out <- getNextMessage h
              if BSL.null out
                then return ()
                else do
                  reqMap <- readMVar reqVar
                  void $ trigger (decodeFromServerMsg reqMap out)
                  go
      return go
    err h trigger = do
      H.hSetBuffering h H.LineBuffering
      let go = do
            open <- H.hIsOpen h
            readable <- H.hIsReadable h
            when (open && readable) $ do
              out <- BS.hGetSome h 3200
              if BS.null out
                then return ()
                else do
                  void $ trigger out
                  go
      return go
