{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module handles setting up the server and handling client requests,
-- functioning as the core that connects all components.

module Server (
  initialState, runServer
) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad (when)
import           Data.ByteString.Char8 (ByteString, append)

import           Control.Lens (makeLenses, view)
import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, recv, send, serve
  )

import           Command
import           LimitedHashMap (LimitedHashMap, initialLHM)

-- | Hold all state that the server has
data ServerState = ServerState
  { _debug :: !Bool                  -- ^ Debug output?
  , _lhm   :: !(MVar LimitedHashMap) -- ^ The hashmap
  }

makeLenses ''ServerState

-- | Set up the initial state
initialState :: Bool -> Int -> IO ServerState
initialState dbg size = do
  lhm <- newMVar $ initialLHM size
  return $ ServerState dbg lhm

-- | Run the server on the specified port
runServer :: ServerState -> Word -> IO ()
runServer state port = do
  mst <- newMVar state
  debugP mst $ "Listening on port " ++ show port
  serve (Host "127.0.0.1") (show port) $ handle mst

-- | Handle an incoming connection
handle :: MVar ServerState -> (Socket, SockAddr) -> IO ()
handle state (sock, remoteAddr) = do
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> do
      debugP state $ "Incoming connection from " ++ show remoteAddr
      debugP state $ "Received message: " ++ show msg
      let cmd = parse msg
      case cmd of
        Left err -> do
          answer sock err
          handle state (sock, remoteAddr)
        Right c  -> do
          lhm <- view lhm <$> readMVar state
          result <- executeCommand lhm c
          answer sock result
          handle state (sock, remoteAddr)

-- | Write an answer to a socket. Appends the correct line-ending
answer :: Socket -> ByteString -> IO ()
answer sock msg = send sock $ append msg "\r\n"

-- | Print debug output if enabled
debugP :: MVar ServerState -> String -> IO ()
debugP state msg = do
  enabled <- view debug <$> readMVar state
  when enabled . putStrLn . ("[DEBUG] " ++) $ msg

