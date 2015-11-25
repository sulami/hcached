{-# LANGUAGE OverloadedStrings #-}

-- | This module handles all the parsing done and therefore defines the exposed
-- interface the clients can access.

module Command (
  Command (..),
  executeCommand, parse
) where

import           Prelude hiding (concat, length, unlines, unwords)

import           Control.Applicative ((<|>), (<*>), (<*), liftA)
import           Control.Monad (forM, liftM)
import           Control.Concurrent.MVar (MVar)
import           Data.ByteString.Char8 (
  ByteString, concat, length, pack, unlines, unpack, unwords
  )
import           Data.Word (Word8)

import           Control.Lens ((^.))
import qualified Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import           Data.Time.Clock.POSIX (POSIXTime)

import           LimitedHashMap

-- | The possible commands and their structure
data Command = SetCmd ByteString Int POSIXTime Bool ByteString
             | GetCmd [ByteString]
             | DelCmd ByteString Bool
             | FlushCmd POSIXTime Bool
             deriving (Eq, Show)

-- | Execute a command and return the answer for the client
executeCommand :: MVar LimitedHashMap -> Command -> IO ByteString
executeCommand lhm (SetCmd k f t n v) = do
  set lhm k f t v
  if n
    then return ""
    else return "STORED"
executeCommand lhm (GetCmd ks) = liftM (concat . (++ ["END"])) . forM ks $
  \k -> do
  rv <- get lhm k
  case rv of
    Nothing  -> return ""
    Just val -> do
      let brint = pack . show
          flags = brint $ fst val
          value = snd val
          size = brint $ length value
          item = unwords ["VALUE", k, flags, size]
      return $ concat [item, "\r\n", value, "\r\n"]
executeCommand lhm (DelCmd k n) = do
  rv <- get lhm k
  case rv of
    Nothing  -> return $ if n then "" else "NOT_FOUND"
    Just val -> do
      delete lhm k
      return $ if n then "" else "DELETED"
executeCommand lhm (FlushCmd t n) =
  return $ if n then "" else "OK"

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
    AP.Done r "set"       -> useParser setParser r
    AP.Done r "get"       -> useParser getParser r
    AP.Done r "gets"      -> useParser getParser r
    AP.Done r "delete"    -> useParser delParser r
    AP.Done r "flush_all" -> useParser flushParser r
    _                     -> Left "ERROR invalid command"

-- | Parse the initial command word
commandParser :: AP.Parser ByteString
commandParser = AP.takeWhile1 isLowercaseChar
  where
    isLowercaseChar :: Word8 -> Bool
    isLowercaseChar 95 = True -- underscore
    isLowercaseChar w  = w >= 97 && w <= 122

-- | Use a parser to try to parse a command
useParser :: CommandParser -> ByteString -> CommandParseResult
useParser parser msg = do
  let cmd = AP.parse parser msg
  case cmd of
    AP.Done _ rv -> Right rv
    _            -> Left "CLIENT_ERROR invalid arguments"

-- | Set a key-value-pair
setParser :: CommandParser
setParser = do
  k <- char8 ' ' *> AP.takeWhile1 isToken <* char8 ' '
  f <- aNumber <* char8 ' '
  t <- liftA realToFrac aNumber <* char8 ' '
  l <- aNumber -- no space here, if noreply is not set, a newline follows
  r <- noreply <* endOfLine
  v <- AP.take l <* endOfLine
  return $ SetCmd k f t r v

-- | Get a value for a key
getParser :: CommandParser
getParser = do
  k0 <- char8 ' ' *> AP.many' (AP.takeWhile1 isToken <* char8 ' ')
  k1 <- AP.takeWhile1 isToken <* endOfLine
  return . GetCmd $ k0 ++ [k1]

-- | Delete a KVP
delParser :: CommandParser
delParser = DelCmd
  <$> (char8 ' ' *> AP.takeWhile1 isToken)
  <*> noreply <* endOfLine

flushParser :: CommandParser
flushParser = FlushCmd
  <$> liftA realToFrac (AP.option 0 (char8 ' ' *> aNumber))
  <*> noreply <* endOfLine

-- | Parse a possible "noreply" postfix
noreply :: AP.Parser Bool
noreply = AP.option False $ const True <$> AP.string " noreply"

-- | Is a 'Word8' part of the accepted set of characters?
isToken :: Word8 -> Bool
isToken w = w >= 33 && w <= 126

-- | Parse a number
aNumber :: AP.Parser Int
aNumber = read . unpack <$> AP.takeWhile1 isNumber
  where
    isNumber :: Word8 -> Bool
    isNumber w = w >= 48 && w <= 57

