{-# LANGUAGE OverloadedStrings #-}

-- | This module handles all the parsing done and therefore defines the exposed
-- interface the clients can access.

module Command (
  Command (..),
  executeCommand, parse
) where

import           Control.Applicative ((<|>), (<*>), (<*), liftA)
import           Control.Concurrent.MVar (MVar)
import           Data.ByteString.Char8 (ByteString, append, unpack)
import           Data.Word (Word8)

import           Control.Lens ((^.))
import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import           Data.Time.Clock.POSIX (POSIXTime)

import           LimitedHashMap

-- | The possible commands and their structure
data Command = SetCmd ByteString Int POSIXTime ByteString
             | GetCmd ByteString
             | DelCmd ByteString
             deriving (Eq, Show)

-- | Execute a command and return the answer for the client
executeCommand :: MVar LimitedHashMap -> Command -> IO ByteString
executeCommand lhm (SetCmd k f t v) = do
  set lhm k f t v
  return "STORED"
executeCommand lhm (GetCmd k) = do
  rv <- get lhm k
  case rv of
    Nothing  -> return "NOT_FOUND"
    Just val -> return . append "VALUE " $ val
executeCommand lhm (DelCmd k) = do
  rv <- get lhm k
  case rv of
    Nothing  -> return "NOT_FOUND"
    Just val -> do
      delete lhm k
      return "DELETED"

-- | What parser functions look like
type CommandParser = AP.Parser Command

-- | Either a valid command or an error message for the client
type CommandParseResult = Either ByteString Command

-- | Parse a received message and return a parse result, which can be the
-- command or an error message for the client
parse :: ByteString -> CommandParseResult
parse msg = do
  let cmd = AP.parse commandParser msg
  case cmd of
    AP.Done r "set"    -> useParser setParser r
    AP.Done r "get"    -> useParser getParser r
    AP.Done r "delete" -> useParser delParser r
    _                  -> Left "CLIENT_ERROR invalid command"

-- | Parse the initial command word
commandParser :: AP.Parser ByteString
commandParser = AP.takeWhile1 isLowercaseChar <* char8 ' '
  where
    isLowercaseChar :: Word8 -> Bool
    isLowercaseChar w = w >= 97 && w <= 122

-- | Use a parser to try to parse a command
useParser :: CommandParser -> ByteString -> CommandParseResult
useParser parser msg = do
  let cmd = AP.parse parser msg
  case cmd of
    AP.Done _ rv -> Right rv
    _            -> Left "CLIENT_ERROR invalid arguments"

-- | Set a key-value-pair
setParser :: CommandParser
setParser = SetCmd
  <$> AP.takeWhile1 isToken <* char8 ' ' <*> aNumber
  <*> liftA toPosixTime (AP.takeWhile1 isNumber) <* char8 ' '
  <*> (AP.take =<< aNumber) <* endOfLine
  where
    aNumber :: AP.Parser Int
    aNumber = read . unpack <$> AP.takeWhile1 isNumber <* char8 ' '

    isNumber :: Word8 -> Bool
    isNumber w = w >= 48 && w <= 57

    toPosixTime :: ByteString -> POSIXTime
    toPosixTime = realToFrac . read . unpack

-- | Get a value for a key
getParser :: CommandParser
getParser = GetCmd <$> AP.takeWhile1 isToken <* endOfLine

-- | Delete a KVP
delParser :: CommandParser
delParser = DelCmd <$> AP.takeWhile1 isToken <* endOfLine

-- | Is a 'Word8' part of the accepted set of characters?
isToken :: Word8 -> Bool
isToken w = w >= 33 && w <= 126

