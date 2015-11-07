module Server (
  runServer
) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)

import           Network.Simple.TCP (
  HostPreference (Host), Socket, SockAddr, serve
  )

import           LimitedHashMap

-- | Run the server on the specified port
runServer :: Bool -> Word -> LHM ()
runServer debug port = do
  liftIO $ when debug . putStrLn $ "Listening on port " ++ show port
  liftIO $ serve (Host "127.0.0.1") (show port) handler

-- | Handle an incoming connection
handler :: (Socket, SockAddr) -> IO ()
handler (sock, remoteAddr) = do
  putStrLn $ "Incoming connection from " ++ show remoteAddr

