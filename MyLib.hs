{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib (toBS, run) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Binary
import Network.Socket
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Concurrent.Async (async)
import Network.Socket.ByteString (recv, sendAll)

toBS :: String -> B.ByteString
toBS s = BL.toStrict $ mconcat
  [ encode (fromIntegral (length s) :: Int32)
  , BL.fromStrict $ BC8.pack s
  ]

port :: Int
port = 9922

socketMaxQueue :: Int
socketMaxQueue = 1024

socketMaxRec :: Int
socketMaxRec = 1024


prepareSocket :: Socket -> AddrInfo -> IO Socket
prepareSocket socket addr@AddrInfo{..} = do
  setSocketOption socket ReuseAddr 1
  bind socket addrAddress
  listen socket socketMaxQueue
  pure socket


run :: IO ()
run = withSocketsDo do
  addr <- head <$> getAddrInfo
    do Just defaultHints {addrSocketType = Stream}
    do Just "127.0.0.1"
    do Just $ show port
  E.bracketOnError (openSocket addr) close \socket -> do
    prepareSocket socket addr >>= loop


loop :: Socket -> IO ()
loop socket = do
  forever $ E.bracketOnError (fst <$> accept socket) close \conn -> do
    void $ async do
      sendAll conn $ toBS "HELLO"
      go conn where
        go conn = do
          msg <- recv conn socketMaxRec
          unless (B.null msg) do
            sendAll conn $ toBS "HI"
            go conn
          close conn
