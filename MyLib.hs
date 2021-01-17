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
import Options.Applicative


totalConn :: TVar Int
totalConn = unsafePerformIO $ newTVarIO 0
{-# NOINLINE totalConn #-}

activeConn :: TVar Int
activeConn = unsafePerformIO $ newTVarIO 0
{-# NOINLINE activeConn #-}


data Opts = Opts
  { port :: Int
  , socketMaxQueue :: Int
  , socketMaxRec :: Int
  , acceptMessage :: String
  , receiveMessage :: String
  , constDelayAccept :: Int
  , constDelayReceive :: Int
  , totalCoefficientAccept :: Int
  , totalCoefficientReceive :: Int
  , activeCoefficientAccept :: Int
  , activeCoefficientReceive :: Int
  }

data ConnectionStatus = Active | Inactive
  deriving Eq

data Connection = Connection
  { status :: TVar ConnectionStatus
  , socket :: Socket
  }


prepareSocket :: Opts -> Socket -> AddrInfo -> IO Socket
prepareSocket Opts{..} socket addr@AddrInfo{..} = do
  setSocketOption socket ReuseAddr 1
  bind socket addrAddress
  listen socket socketMaxQueue
  pure socket


debugLogger :: Opts -> IO ()
debugLogger opts = do
  total <- readTVarIO totalConn
  active <- readTVarIO activeConn
  acceptDelay <- getAcceptDelay opts
  receiceDelay <- getReceiveDelay opts
  putStrLn $ mconcat
    [ "total " , show total, ", "
    , "active ", show active, ", "
    , "accept delay ", show acceptDelay, ", "
    , "receive delay ", show receiceDelay
    ]
  threadDelay 1_000_000
  debugLogger opts


optParser :: ParserInfo Opts
optParser = info
  do helper <*> programOptions
  do mconcat
      [ fullDesc
      , progDesc $ mconcat
          [ "Dummy tcp server.\n"
          , "Delay = const + [total connections] * total_coeff + [active connections] * active_coeff"
          ]
      ]
    where
      programOptions = Opts
        <$> option auto do mconcat [long "port", value 9922]
        <*> option auto do mconcat [long "socket_max_queue", value 1024]
        <*> option auto do mconcat [long "socket_max_receive", value 1024]
        <*> strOption do mconcat [long "accept_message", value "HELLO"]
        <*> strOption do mconcat [long "receive_message", value "HI"]
        <*> option auto do mconcat [long "const_delay_accept", value 0]
        <*> option auto do mconcat [long "const_delay_receive", value 0]
        <*> option auto do mconcat [long "total_coeff_accept", value 0]
        <*> option auto do mconcat [long "total_coeff_receive", value 0]
        <*> option auto do mconcat [long "active_coeff_accept", value 0]
        <*> option auto do mconcat [long "active_coeff_receive", value 0]


run :: IO ()
run = withSocketsDo do
  opts@Opts{..} <- execParser optParser
  async $ debugLogger opts
  addr <- head <$> getAddrInfo
    do Just defaultHints {addrSocketType = Stream}
    do Just "127.0.0.1"
    do Just $ show port
  E.bracketOnError (openSocket addr) close \socket -> do
    prepareSocket opts socket addr >>= loop opts


loop :: Opts -> Socket -> IO ()
loop opts@Opts{..} socket' = do
  forever $ E.bracketOnError (newConn socket') closeConn \conn -> do
    void $ async do
      getAcceptDelay opts >>= threadDelay
      withSocket conn (`sendAll` toBS acceptMessage)
      go conn
      closeConn conn
      where
        go conn = do
          msg <- withSocket conn (`recv` socketMaxRec)
          unless (B.null msg) do
            getReceiveDelay opts >>= threadDelay
            activateConn conn
            withSocket conn (`sendAll` toBS receiveMessage)
            go conn


toBS :: String -> B.ByteString
toBS s = BL.toStrict $ mconcat
  [ encode @Int32 $ fromIntegral $ length s
  , BL.fromStrict $ BC8.pack s
  ]

getAcceptDelay :: Opts -> IO Int
getAcceptDelay Opts{..} = do
  total <- readTVarIO totalConn
  active <- readTVarIO activeConn
  pure $ calculateDelay total totalCoefficientAccept active activeCoefficientAccept constDelayAccept

getReceiveDelay :: Opts -> IO Int
getReceiveDelay Opts{..} = do
  total <- readTVarIO totalConn
  active <- readTVarIO activeConn
  pure $ calculateDelay total totalCoefficientReceive active activeCoefficientReceive constDelayReceive



calculateDelay :: Int -> Int -> Int -> Int -> Int -> Int
calculateDelay total totalCoeff active activeCoeff c =
  c + total * totalCoeff + active * activeCoeff

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
