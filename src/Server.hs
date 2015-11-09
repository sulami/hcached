{-# LANGUAGE OverloadedStrings #-}

module Server (
  runServer
) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C8

import           Control.Lens (view)
import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, acceptFork, recv, send, serve
  )

import           LimitedHashMap

-- | Run the server on the specified port
runServer :: LimitedHashMap -> Word -> IO ()
runServer state port = do
  lhm <- newMVar state
  debugP lhm $ "Listening on port " ++ show port
  serve (Host "127.0.0.1") (show port) $ handle lhm

  -- | Handle an incoming connection
handle :: MVar LimitedHashMap -> (Socket, SockAddr) -> IO ()
handle lhm (sock, remoteAddr) = do
  debugP lhm $ "Incoming connection from " ++ show remoteAddr
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> parse lhm sock msg

-- | Parse a received message and act accordingly. Validity checking is done in
-- the individual parsers
parse :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parse lhm sock msg = do
  debugP lhm $ "Received message: " ++ show msg
  let cmds = C8.words msg
  case head cmds of
    "set" -> parseSet lhm sock $ drop 1 cmds
    "get" -> parseGet lhm sock $ drop 1 cmds
    _     -> send sock "ERROR"

-- | Set a key-value-pair
parseSet :: MVar LimitedHashMap -> Socket -> [C8.ByteString] -> IO ()
parseSet lhm sock msg = do
  if length msg < 2
    then send sock "CLIENT_ERROR"
    else do
      insert lhm (head msg) (C8.unwords $ drop 1 msg)
      send sock "STORED"

-- | Get a value for a key
parseGet :: MVar LimitedHashMap -> Socket -> [C8.ByteString] -> IO ()
parseGet lhm sock msg = do
  if length msg /= 1
    then send sock "CLIENT_ERROR"
    else do
      rv <- query lhm $ head msg
      case rv of
        Nothing  -> send sock "NOT_FOUND"
        Just val -> send sock $ C8.unwords ["VALUE", val]

-- | Print debug output if enabled
debugP :: MVar LimitedHashMap -> String -> IO ()
debugP lhm msg = do
  enabled <- (view debug) <$> readMVar lhm
  when enabled . putStrLn . ("[DEBUG] " ++) $ msg

