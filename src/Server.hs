{-# LANGUAGE OverloadedStrings #-}

module Server (
  runServer
) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (get)
import qualified Data.ByteString.Char8 as C8

import           Control.Lens (view)
import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, accept, listen, recv, send
  )

import           LimitedHashMap

-- | Run the server on the specified port
runServer :: Word -> LHM ()
runServer port = do
  debugP $ "Listening on port " ++ show port
  listen (Host "127.0.0.1") (show port) hear

-- | Loop and "listen"
hear :: (Socket, SockAddr) -> LHM ()
hear (sock, addr) = do
  accept sock handle
  hear (sock, addr)

  -- | Handle an incoming connection
handle :: (Socket, SockAddr) -> LHM ()
handle (sock, remoteAddr) = do
  debugP $ "Incoming connection from " ++ show remoteAddr
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> parse sock msg

-- | Parse a received message and act accordingly
parse :: Socket -> C8.ByteString -> LHM ()
parse sock msg = do
  debugP $ "Received message: " ++ show msg
  let cmds = C8.words msg
  case head cmds of
    "set" -> insert (cmds !! 1) (cmds !! 2)
    "get" -> do
      rv <- query (cmds !! 1)
      case rv of
        Nothing  -> send sock "NOTFOUND"
        Just val -> send sock val
    _ -> return ()

-- | Print debug output if enabled
debugP :: String -> LHM ()
debugP msg = do
  enabled <- (view debug) <$> get
  when enabled . liftIO . putStrLn . ("[DEBUG] " ++) $ msg

