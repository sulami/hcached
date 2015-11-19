{-# LANGUAGE OverloadedStrings #-}

module Server (
  runServer
) where

import           Control.Applicative ((<|>), (*>), (<*))
import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as C8
import           Data.Char (isDigit)

import           Control.Lens (view)
import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 (char8)
import           Data.Time.Clock (secondsToDiffTime)
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
data Command = SetCmd C8.ByteString C8.ByteString
             | GetCmd C8.ByteString
             | DelCmd C8.ByteString

-- | Parse a received message and act accordingly. Validity checking is done in
-- the individual parsers
parse :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parse lhm sock msg = do
  debugP lhm $ "Received message: " ++ show msg
  let cmd = AP.parse commandParser msg
  case cmd of
    AP.Done "set"    r -> parseSet lhm sock r
    AP.Done "get"    r -> parseSet lhm sock r
    AP.Done "delete" r -> parseSet lhm sock r
    _                  -> answer sock $ invalidCommand msg
  where
    commandParser :: AP.Parser C8.ByteString
    commandParser = AP.takeWhile1 (AP.inClass "a-z") <* char8 ' '

    invalidCommand :: C8.ByteString -> C8.ByteString
    invalidCommand = C8.append "CLIENT_ERROR invalid command: "

-- | Set a key-value-pair
parseSet :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseSet lhm sock msg = undefined --do
  -- if length msg < 3
  --   then answer sock "CLIENT_ERROR insufficient arguments"
  --   else do
  --     let time = C8.unpack $ msg !! 1
  --     if all isDigit time -- TODO extract this/parser
  --       then do
  --         set lhm (head msg) (C8.unwords $ drop 2 msg) (realToFrac $ read time)
  --         answer sock "STORED"
  --       else answer sock "CLIENT ERROR invalid time format"

-- | Get a value for a key
parseGet :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseGet lhm sock msg = undefined --do
  -- if length msg /= 1
  --   then answer sock "CLIENT_ERROR invalid arguments"
  --   else do
  --     rv <- get lhm $ head msg
  --     case rv of
  --       Nothing  -> answer sock "NOT_FOUND"
  --       Just val -> answer sock $ C8.unwords ["VALUE", view value val]

-- | Delete a KVP
parseDel :: MVar LimitedHashMap -> Socket -> C8.ByteString -> IO ()
parseDel lhm sock msg = undefined --do
  -- if length msg /= 1
  --   then answer sock "CLIENT_ERROR invalid arguments"
  --   else do
  --     rv <- get lhm $ head msg
  --     case rv of
  --       Nothing -> answer sock "NOT_FOUND"
  --       _       -> do
  --         delete lhm $ head msg
  --         answer sock "DELETED"

-- | Write an answer to a socket. Appends the correct line-ending
answer :: Socket -> C8.ByteString -> IO ()
answer sock msg = send sock $ C8.concat [msg, "\r\n"]

-- | Print debug output if enabled
debugP :: MVar LimitedHashMap -> String -> IO ()
debugP lhm msg = do
  enabled <- (view debug) <$> readMVar lhm
  when enabled . putStrLn . ("[DEBUG] " ++) $ msg

