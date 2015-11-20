{-# LANGUAGE OverloadedStrings #-}

-- | This module handles all the parsing done and therefore defines the exposed
-- interface the clients can access.

module Command (
  Command (..),
  parse
) where

import           Control.Applicative ((<|>), (<*>), (<*), liftA)
import           Data.ByteString.Char8 (ByteString, unpack)

import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import           Data.Time.Clock.POSIX (POSIXTime)

-- | The possible commands and their structure
data Command = SetCmd POSIXTime ByteString ByteString
             | GetCmd ByteString
             | DelCmd ByteString
             deriving (Show)

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
commandParser = AP.takeWhile1 (AP.inClass "a-z") <* char8 ' '

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
  <$> (liftA toPosixTime . AP.takeWhile $ AP.inClass "0-9") <* char8 ' '
  <*> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* char8 ' '
  <*> AP.takeWhile1 (\c -> c /= 13 && c /= 10) <* endOfLine
  where
    toPosixTime :: ByteString -> POSIXTime
    toPosixTime = realToFrac . read . unpack

-- | Get a value for a key
getParser :: CommandParser
getParser = GetCmd <$> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* endOfLine

-- | Delete a KVP
delParser :: CommandParser
delParser = DelCmd <$> AP.takeWhile1 (AP.inClass "a-zA-Z0-9") <* endOfLine

