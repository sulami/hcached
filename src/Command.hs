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
             | AddCmd ByteString Int POSIXTime Bool ByteString
             | ReplaceCmd ByteString Int POSIXTime Bool ByteString
             | AppendCmd ByteString Bool ByteString
             | PrependCmd ByteString Bool ByteString
             | GetCmd [ByteString]
             | DelCmd ByteString Bool
             | TouchCmd ByteString POSIXTime Bool
             | FlushCmd POSIXTime Bool
             deriving (Eq, Show)

-- | Execute a command and return the answer for the client
executeCommand :: MVar LimitedHashMap -> Command -> IO ByteString
executeCommand lhm cmd = case cmd of
  SetCmd k f t n v -> do
    set lhm k f t v
    reply n "STORED"
  AddCmd k f t n v -> do
    mem <- isMember lhm k
    if mem
      then reply n "NOT_STORED"
      else do
        set lhm k f t v
        reply n "STORED"
  ReplaceCmd k f t n v -> do
    mem <- isMember lhm k
    if not mem
      then reply n "NOT_STORED"
      else do
        set lhm k f t v
        reply n "STORED"
  AppendCmd k n v -> do
    mem <- isMember lhm k
    if not mem
      then reply n "NOT_STORED"
      else do
        append lhm k v
        reply n "STORED"
  PrependCmd k n v -> do
    mem <- isMember lhm k
    if not mem
      then reply n "NOT_STORED"
      else do
        prepend lhm k v
        reply n "STORED"
  GetCmd ks -> liftM (concat . (++ ["END"])) . forM ks $
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
  DelCmd k n -> do
    mem <- isMember lhm k
    if not mem
      then reply n "NOT_FOUND"
      else do
        delete lhm k
        reply n "DELETED"
  TouchCmd k t n -> do
    mem <- isMember lhm k
    if not mem
      then reply n "NOT_FOUND"
      else do
        touch lhm k t
        reply n "TOUCHED"
  FlushCmd t n -> do
    flush lhm t
    reply n "OK"

-- | Reply with a given message, or if noreply is set, with nothing
reply :: Bool -> ByteString -> IO ByteString
reply True = return . const ""
reply False = return

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
    AP.Done r "add"       -> useParser addParser r
    AP.Done r "replace"   -> useParser replaceParser r
    AP.Done r "append"    -> useParser appendParser r
    AP.Done r "prepend"   -> useParser prependParser r
    AP.Done r "get"       -> useParser getParser r
    AP.Done r "gets"      -> useParser getParser r
    AP.Done r "delete"    -> useParser delParser r
    AP.Done r "touch"     -> useParser touchParser r
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
setParser = insertionUncurry SetCmd <$> insertionParser

-- | Set a key-value-pair only if not already present
addParser :: CommandParser
addParser = insertionUncurry AddCmd <$> insertionParser

-- | Set a key-value-pair only if already present
replaceParser :: CommandParser
replaceParser = insertionUncurry ReplaceCmd <$> insertionParser

-- | The arguments all insertion-based commands take
type InsertionArgs = (ByteString, Int, POSIXTime, Bool, ByteString)

-- | The type of an insertion-based command's constructor
type InsertionCommand =
  ByteString -> Int -> POSIXTime -> Bool -> ByteString -> Command

-- | Uncurry the insertion arguments for command construction
insertionUncurry :: InsertionCommand -> InsertionArgs -> Command
insertionUncurry cmd (k,f,t,r,v) = cmd k f t r v

-- | Parse the arguments for all the insertion-based commands
insertionParser :: AP.Parser InsertionArgs
insertionParser = do
  k <- keyParser
  f <- aNumber <* char8 ' '
  t <- liftA realToFrac aNumber <* char8 ' '
  (r, v) <- sizedContentParser
  return (k, f, t, r, v)

-- | Append a value to an existing one
appendParser :: CommandParser
appendParser = do
  k <- keyParser
  (r, v) <- sizedContentParser
  return $ AppendCmd k r v

-- | Prepend a value to an existing one
prependParser :: CommandParser
prependParser = do
  k <- keyParser
  (r, v) <- sizedContentParser
  return $ PrependCmd k r v

-- | Parse size-annotated content blocks (and optional noreply flags)
sizedContentParser :: AP.Parser (Bool, ByteString)
sizedContentParser = do
  l <- aNumber -- no space here, if noreply is not set, a newline follows
  r <- noreplyParser <* endOfLine
  v <- AP.take l <* endOfLine
  return (r, v)

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
  <*> noreplyParser <* endOfLine

-- | Update just the TTL of a value
touchParser :: CommandParser
touchParser = TouchCmd
  <$> keyParser <*> liftA realToFrac aNumber <*> noreplyParser <* endOfLine

-- | Delete all KVPs that are valid longer than t (all if t == 0)
flushParser :: CommandParser
flushParser = FlushCmd
  <$> liftA realToFrac (AP.option 0 (char8 ' ' *> aNumber))
  <*> noreplyParser <* endOfLine

-- | Parse a key including surrounding whitespace on both sides
keyParser :: AP.Parser ByteString
keyParser = char8 ' ' *> AP.takeWhile1 isToken <* char8 ' '

-- | Parse a possible "noreply" postfix
noreplyParser :: AP.Parser Bool
noreplyParser = AP.option False $ const True <$> AP.string " noreply"

-- | Is a 'Word8' part of the accepted set of characters?
isToken :: Word8 -> Bool
isToken w = w >= 33 && w <= 126

-- | Parse a number
aNumber :: AP.Parser Int
aNumber = read . unpack <$> AP.takeWhile1 isNumber
  where
    isNumber :: Word8 -> Bool
    isNumber w = w >= 48 && w <= 57

