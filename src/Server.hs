{-# LANGUAGE OverloadedStrings #-}

module Server (
  runServer
) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (get)
import qualified Data.ByteString.Char8 as C8

import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, accept, listen, recv, send
  )

import           LimitedHashMap

-- | Run the server on the specified port
runServer :: Bool -> Word -> LHM ()
runServer debug port = do
  liftIO . when debug . putStrLn $ "Listening on port " ++ show port
  listen (Host "127.0.0.1") (show port) (listener debug)

-- | Loop and listen
listener :: Bool -> (Socket, SockAddr) -> LHM ()
listener debug (sock, addr) = do
  accept sock $ handler debug
  listener debug (sock, addr)

  -- | Handle an incoming connection
handler :: Bool -> (Socket, SockAddr) -> LHM ()
handler debug (sock, remoteAddr) = do
  when debug . liftIO . putStrLn $
    "Incoming connection from " ++ show remoteAddr
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> parse debug sock msg

-- | Parse a received message and act accordingly
parse :: Bool -> Socket -> C8.ByteString -> LHM ()
parse debug sock msg = do
  when debug . liftIO . putStrLn $
    "Received message: " ++ show msg
  let cmds = C8.words msg
  case head cmds of
    "set" -> insert (cmds !! 1) (cmds !! 2)
    "get" -> do
      rv <- query (cmds !! 1)
      case rv of
        Nothing  -> send sock "NOTFOUND"
        Just val -> send sock val
    _ -> return ()

