{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module MyLib (toBS, run) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.Bool (bool)
import Data.Int
import Data.Binary
import Network.Socket
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Concurrent.Async (async)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent.STM
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import System.IO.Unsafe (unsafePerformIO)

port :: Int
port = 9922

socketMaxQueue :: Int
socketMaxQueue = 1024

socketMaxRec :: Int
socketMaxRec = 1024

acceptMessage :: B.ByteString
acceptMessage = toBS "HELLO"

receiveMessage :: B.ByteString
receiveMessage = toBS "HI"

totalConn :: TVar Int
totalConn = unsafePerformIO $ newTVarIO 0
{-# NOINLINE totalConn #-}

activeConn :: TVar Int
activeConn = unsafePerformIO $ newTVarIO 0
{-# NOINLINE activeConn #-}


data ConnectionStatus = Active | Inactive
  deriving Eq

data Connection = Connection
  { status :: TVar ConnectionStatus
  , socket :: Socket
  }


prepareSocket :: Socket -> AddrInfo -> IO Socket
prepareSocket socket addr@AddrInfo{..} = do
  setSocketOption socket ReuseAddr 1
  bind socket addrAddress
  listen socket socketMaxQueue
  pure socket


debugLogger :: IO ()
debugLogger = do
  total <- readTVarIO totalConn
  active <- readTVarIO activeConn
  putStrLn $ mconcat ["total - ", show total, ", active - ", show active]
  threadDelay 1_000_000
  debugLogger


run :: IO ()
run = withSocketsDo do
  forkIO debugLogger
  addr <- head <$> getAddrInfo
    do Just defaultHints {addrSocketType = Stream}
    do Just "127.0.0.1"
    do Just $ show port
  E.bracketOnError (openSocket addr) close \socket -> do
    prepareSocket socket addr >>= loop


loop :: Socket -> IO ()
loop socket' = do
  forever $ E.bracketOnError (newConn socket') closeConn \conn -> do
    void $ async do
      withSocket conn (`sendAll` acceptMessage)
      go conn
      closeConn conn
      where
        go conn = do
          msg <- withSocket conn (`recv` socketMaxRec)
          unless (B.null msg) do
            activateConn conn
            withSocket conn (`sendAll` receiveMessage)
            go conn


toBS :: String -> B.ByteString
toBS s = BL.toStrict $ mconcat
  [ encode @Int32 $ fromIntegral $ length s
  , BL.fromStrict $ BC8.pack s
  ]

withSocket :: Connection -> (Socket -> IO a) -> IO a
withSocket Connection{..} f = f socket

newConn :: Socket -> IO Connection
newConn socket' = do
  incTotalConn
  socket <- fst <$> accept socket'
  status <- newTVarIO Inactive
  pure Connection {..}

activateConn :: Connection -> IO ()
activateConn c@Connection{..} = connIsActive c >>= bool
  do atomically (writeTVar status Active) >> incActiveConn
  do pure ()

connIsActive :: Connection -> IO Bool
connIsActive Connection{..} = readTVarIO status <&> (== Active)

closeConn :: Connection -> IO ()
closeConn c@Connection{..} = do
  connIsActive c >>= bool
    do pure ()
    do decActiveConn
  decTotalConn
  close socket

incTotalConn :: IO ()
incTotalConn = atomically $ modifyTVar' totalConn (+1)

decTotalConn :: IO ()
decTotalConn = atomically $ modifyTVar' totalConn (\x -> x -1)

incActiveConn :: IO ()
incActiveConn = atomically $ modifyTVar' activeConn (+1)

decActiveConn :: IO ()
decActiveConn = atomically $ modifyTVar' activeConn (\x -> x -1)
