{-# LANGUAGE OverloadedStrings #-}

-- | This module handles setting up the server and handling client requests,
-- functioning as the core that connects all components.

module Server (
  runServer
) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C8

import           Control.Lens (view)
import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, recv, send, serve
  )

import           Command
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
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> do
      debugP lhm $ "Incoming connection from " ++ show remoteAddr
      debugP lhm $ "Received message: " ++ show msg
      let cmd = parse msg
      case cmd of
        Left err -> do
          answer sock err
          handle lhm (sock, remoteAddr)
        Right c  -> do
          result <- executeCommand lhm c
          answer sock result
          handle lhm (sock, remoteAddr)

-- | Write an answer to a socket. Appends the correct line-ending
answer :: Socket -> C8.ByteString -> IO ()
answer sock msg = send sock $ C8.concat [msg, "\r\n"]

-- | Print debug output if enabled
debugP :: MVar LimitedHashMap -> String -> IO ()
debugP lhm msg = do
  enabled <- view debug <$> readMVar lhm
  when enabled . putStrLn . ("[DEBUG] " ++) $ msg

