{-# LANGUAGE OverloadedStrings #-}

module Server (
  runServer
) where

import           Control.Applicative ((<|>), (<*>), (<*), liftA)
import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C8

import           Control.Lens (view)
import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import           Data.Time.Clock.POSIX (POSIXTime)
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
  inc <- recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> do
      debugP lhm $ "Incoming connection from " ++ show remoteAddr
      parse lhm sock msg
      handle lhm (sock, remoteAddr)

-- | The possible commands and their structure
data Command = SetCmd POSIXTime C8.ByteString C8.ByteString
             | GetCmd C8.ByteString
             | DelCmd C8.ByteString
             deriving (Show)

-- | Parse a received message and act accordingly. Validity checking is done in
-- the individual parsers
parse :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parse lhm sock msg = do
  debugP lhm $ "Received message: " ++ show msg
  let cmd = AP.parse commandParser msg
  case cmd of
    AP.Done r "set"    -> parseSet lhm sock r
    AP.Done r "get"    -> parseGet lhm sock r
    AP.Done r "delete" -> parseDel lhm sock r
    _                  -> answer sock "CLIENT_ERROR invalid command: "
  where
    commandParser :: AP.Parser C8.ByteString
    commandParser = AP.takeWhile1 (AP.inClass "a-z") <* char8 ' '

-- | Set a key-value-pair
parseSet :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseSet lhm sock msg = do
  let cmd = AP.parse setParser msg
  case cmd of
    AP.Done _ (SetCmd t k v) -> set lhm k v t >> answer sock "STORED"
    _                        -> answer sock "CLIENT_ERROR invalid arguments"
  where
    setParser :: AP.Parser Command
    setParser = SetCmd
      <$> (liftA toPosixTime . AP.takeWhile $ AP.inClass "0-9") <* char8 ' '
      <*> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* char8 ' '
      <*> AP.takeWhile1 (\c -> c /= 13 && c /= 10) <* endOfLine

    toPosixTime :: C8.ByteString -> POSIXTime
    toPosixTime = realToFrac . read . C8.unpack

-- | Get a value for a key
parseGet :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseGet lhm sock msg = do
  let cmd = AP.parse getParser msg
  case cmd of
    AP.Fail _ _ _        -> answer sock "CLIENT_ERROR invalid arguments"
    AP.Done _ (GetCmd k) -> do
      rv <- get lhm k
      case rv of
        Nothing  -> answer sock "NOT_FOUND"
        Just val -> answer sock $ view value val
  where
    getParser :: AP.Parser Command
    getParser = GetCmd <$> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* endOfLine

-- | Delete a KVP
parseDel :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseDel lhm sock msg = do
  let cmd = AP.parse delParser msg
  case cmd of
    AP.Fail _ _ _        -> answer sock "CLIENT_ERROR invalid arguments"
    AP.Done _ (DelCmd k) -> do
      rv <- get lhm k
      case rv of
        Nothing  -> answer sock "NOT_FOUND"
        Just val -> do
          delete lhm k
          answer sock "DELETED"
  where
    delParser :: AP.Parser Command
    delParser = DelCmd <$> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* endOfLine

-- | Write an answer to a socket. Appends the correct line-ending
answer :: Socket -> C8.ByteString -> IO ()
answer sock msg = send sock $ C8.concat [msg, "\r\n"]

-- | Print debug output if enabled
debugP :: MVar LimitedHashMap -> String -> IO ()
debugP lhm msg = do
  enabled <- (view debug) <$> readMVar lhm
  when enabled . putStrLn . ("[DEBUG] " ++) $ msg

