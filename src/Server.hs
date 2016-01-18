{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module handles setting up the server and handling client requests,
-- parsing requests and dispatching commands to the embedded LHM.

module Server where

import           Control.Applicative              ((<*), (<*>), (<|>))
import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Concurrent.MVar          (MVar, newMVar, readMVar)
import           Control.Monad                    (forM, liftM, unless, when)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import           Data.Maybe                       (fromJust)
import           Data.Version                     (showVersion)
import           Data.Word                        (Word64, Word8)

import           Control.Lens                     (makeLenses, over, view, (^.))
import qualified Data.Attoparsec.ByteString       as AP
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import           Data.Time.Clock.POSIX            (POSIXTime)
import qualified Network.Simple.TCP               as TCP
import           System.Posix.Syslog              (Facility (..), Option (..),
                                                   Priority (..), logUpTo,
                                                   syslog, withSyslog)

import           LimitedHashMap
import qualified Paths_hcached                    as P

-- | Hold all state that the server has
data ServerState = ServerState
  { _debug :: !Bool                  -- ^ Debug output?
  , _cui   :: !Int                   -- ^ Janitor cleanup interval in seconds
  , _lhm   :: !(MVar LimitedHashMap) -- ^ The hashmap
  , _ver   :: !C8.ByteString         -- ^ Server version
  }

makeLenses ''ServerState

-- | Set up the initial state
initialState :: Bool -> Int -> Int -> IO ServerState
initialState dbg cui size = do
  lhm <- newMVar $ initialLHM size
  let version = C8.pack $ showVersion P.version
  return $ ServerState dbg cui lhm version

-- | Run the server on the specified port
runServer :: ServerState -> Word -> IO ()
runServer state port = withSyslog "hcached" [PID] USER (logUpTo Debug) $ do
  mst <- newMVar state
  forkIO $ janitor mst
  infoP $ "Starting up, listening on port " ++ show port
  TCP.serve (TCP.Host "0.0.0.0") (show port) $ handle mst

-- | Handle an incoming connection
handle :: MVar ServerState -> (TCP.Socket, TCP.SockAddr) -> IO ()
handle state (sock, remoteAddr) = do
  inc <- TCP.recv sock 256
  case inc of
    Nothing  -> return ()
    Just msg -> do
      debugP state $ "Incoming connection from " ++ show remoteAddr
      debugP state $ "Received message: " ++ show msg
      let cmd = parse msg
      case cmd of
        Left err      -> do
          answer sock err
          handle state (sock, remoteAddr)
        Right QuitCmd -> TCP.closeSock sock
        Right c       -> do
          result <- executeCommand state c
          unless (C8.null result) $ answer sock result
          handle state (sock, remoteAddr)

-- | Perdiodically clean the LHM in a seperate thread
janitor :: MVar ServerState -> IO ()
janitor state = do
  s <- readMVar state
  debugP state "Running janitor job"
  cleanup $ view lhm s
  threadDelay $ view cui s * 1000000
  janitor state

-- | Write an answer to a socket. Appends the correct line-ending
answer :: TCP.Socket -> C8.ByteString -> IO ()
answer sock msg = TCP.send sock $ C8.append msg "\r\n"

-- | Print info output to stdout and the syslog
infoP :: String -> IO ()
infoP msg = do
  syslog Info msg
  putStrLn . ("[INFO] " ++) $ msg

-- | Print debug output to stdout and the syslog if enabled
debugP :: MVar ServerState -> String -> IO ()
debugP state msg = do
  enabled <- view debug <$> readMVar state
  when enabled $ do
     syslog Debug msg
     putStrLn . ("[DEBUG] " ++) $ msg

-- | The possible commands and their structure
data Command
  = SetCmd C8.ByteString Int POSIXTime Bool C8.ByteString
  | AddCmd C8.ByteString Int POSIXTime Bool C8.ByteString
  | ReplaceCmd C8.ByteString Int POSIXTime Bool C8.ByteString
  | AppendCmd C8.ByteString Bool C8.ByteString
  | PrependCmd C8.ByteString Bool C8.ByteString
  | CasCmd C8.ByteString Int POSIXTime Integer Bool C8.ByteString
  | GetCmd [C8.ByteString]
  | DeleteCmd C8.ByteString Bool
  | IncrCmd C8.ByteString Word64 Bool
  | DecrCmd C8.ByteString Word64 Bool
  | TouchCmd C8.ByteString POSIXTime Bool
  | FlushCmd POSIXTime Bool
  | VersionCmd
  | QuitCmd
  deriving (Eq, Show)

-- | Execute a command and return the answer for the client
executeCommand :: MVar ServerState -> Command -> IO C8.ByteString
executeCommand ss cmd = do
  lhm <- view lhm <$> readMVar ss
  case cmd of
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
    CasCmd k f t c n v -> do
      old <- viewUnique lhm k
      case old of
        Nothing  -> reply n "NOT_FOUND"
        Just val -> if c /= val
          then reply n "EXISTS"
          else do
            set lhm k f t v
            reply n "STORED"
    GetCmd ks -> liftM (C8.concat . (++ ["END"])) . forM ks $
      \k -> do
      rv <- get lhm k
      case rv of
        Nothing  -> return ""
        Just val -> do
          let flgs = brint $ val^.flags
              size = brint $ C8.length valu
              casu = brint $ val^.uniq
              item = C8.unwords ["VALUE", k, flgs, size, casu]
              valu = val^.value
          return $ C8.concat [item, "\r\n", valu, "\r\n"]
    DeleteCmd k n -> do
      mem <- isMember lhm k
      if not mem
        then reply n "NOT_FOUND"
        else do
          delete lhm k
          reply n "DELETED"
    IncrCmd k x n -> do
      mem <- isMember lhm k
      if not mem
        then reply n "NOT_FOUND"
        else do
          eligeble <- canInDecr . fromJust <$> get lhm k
          if eligeble
            then incr lhm k x >>= reply n
            else reply n "CLIENT_ERROR cannot increment non-numeric value"
    DecrCmd k x n -> do
      mem <- isMember lhm k
      if not mem
        then reply n "NOT_FOUND"
        else do
          eligeble <- canInDecr . fromJust <$> get lhm k
          if eligeble
            then decr lhm k x >>= reply n
            else reply n "CLIENT_ERROR cannot decrement non-numeric value"
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
    VersionCmd -> view ver <$> readMVar ss

-- | Show and pack, "ByteString-print"
brint :: Show a => a -> C8.ByteString
brint = C8.pack . show

-- | Reply with a given message, or if noreply is set, with nothing
reply :: Bool -> C8.ByteString -> IO C8.ByteString
reply True = return . const ""
reply False = return

-- | What parser functions look like
type CommandParser = AP.Parser Command

-- | Either a valid command or an error message for the client
type CommandParseResult = Either C8.ByteString Command

-- | Parse a received message and return a parse result, which can be the
-- command or an error message for the client
parse :: C8.ByteString -> CommandParseResult
parse msg = do
  let cmd = AP.parse commandParser msg
  case cmd of
    AP.Done r "set"       -> useParser setParser r
    AP.Done r "add"       -> useParser addParser r
    AP.Done r "replace"   -> useParser replaceParser r
    AP.Done r "append"    -> useParser appendParser r
    AP.Done r "prepend"   -> useParser prependParser r
    AP.Done r "cas"       -> useParser casParser r
    AP.Done r "get"       -> useParser getParser r
    AP.Done r "gets"      -> useParser getParser r
    AP.Done r "delete"    -> useParser deleteParser r
    AP.Done r "incr"      -> useParser incrParser r
    AP.Done r "decr"      -> useParser decrParser r
    AP.Done r "touch"     -> useParser touchParser r
    AP.Done r "flush_all" -> useParser flushParser r
    AP.Done r "version"   -> useParser versionParser r
    AP.Done r "quit"      -> useParser quitParser r
    _                     -> Left "ERROR invalid command"

-- | Parse the initial command word
commandParser :: AP.Parser C8.ByteString
commandParser = AP.takeWhile1 isLowercaseChar
  where
    isLowercaseChar :: Word8 -> Bool
    isLowercaseChar 95 = True -- underscore
    isLowercaseChar w  = w >= 97 && w <= 122

-- | Use a parser to try to parse a command
useParser :: CommandParser -> C8.ByteString -> CommandParseResult
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
type InsertionArgs = (C8.ByteString, Int, POSIXTime, Bool, C8.ByteString)

-- | The type of an insertion-based command's constructor
type InsertionCommand =
  C8.ByteString -> Int -> POSIXTime -> Bool -> C8.ByteString -> Command

-- | Uncurry the insertion arguments for command construction
insertionUncurry :: InsertionCommand -> InsertionArgs -> Command
insertionUncurry cmd (k,f,t,r,v) = cmd k f t r v

-- | Parse the arguments for all the insertion-based commands
insertionParser :: AP.Parser InsertionArgs
insertionParser = do
  k <- keyParser
  f <- aNumber <* char8 ' '
  t <- fmap realToFrac aNumber <* char8 ' '
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

-- | Update a value if it has not been updated since the last time
casParser :: CommandParser
casParser = do
  k <- keyParser
  f <- aNumber <* char8 ' '
  t <- fmap realToFrac aNumber <* char8 ' '
  l <- aNumber <* char8 ' '
  c <- toInteger <$> aNumber
  r <- noreplyParser <* endOfLine
  v <- AP.take l <* endOfLine
  return $ CasCmd k f t c r v

-- | Parse size-annotated content blocks (and optional noreply flags)
sizedContentParser :: AP.Parser (Bool, C8.ByteString)
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
deleteParser :: CommandParser
deleteParser = DeleteCmd
  <$> (char8 ' ' *> AP.takeWhile1 isToken)
  <*> noreplyParser <* endOfLine

-- | Increment a value by n
incrParser :: CommandParser
incrParser = IncrCmd
  <$> keyParser <*> fmap fromIntegral aNumber <*> noreplyParser <* endOfLine

-- | Decrement a value by n
decrParser :: CommandParser
decrParser = DecrCmd
  <$> keyParser <*> fmap fromIntegral aNumber <*> noreplyParser <* endOfLine

-- | Update just the TTL of a value
touchParser :: CommandParser
touchParser = TouchCmd
  <$> keyParser <*> fmap realToFrac aNumber <*> noreplyParser <* endOfLine

-- | Delete all KVPs that are valid longer than t (all if t == 0)
flushParser :: CommandParser
flushParser = FlushCmd
  <$> fmap realToFrac (AP.option 0 (char8 ' ' *> aNumber))
  <*> noreplyParser <* endOfLine

-- | Query the version string of the server
versionParser :: CommandParser
versionParser = endOfLine >> return VersionCmd

-- | Announce the end of the interaction
quitParser :: CommandParser
quitParser = endOfLine >> return QuitCmd

-- | Parse a key including surrounding whitespace on both sides
keyParser :: AP.Parser C8.ByteString
keyParser = char8 ' ' *> AP.takeWhile1 isToken <* char8 ' '

-- | Parse a possible "noreply" postfix
noreplyParser :: AP.Parser Bool
noreplyParser = AP.option False $ const True <$> AP.string " noreply"

-- | Is a 'Word8' part of the accepted set of characters?
isToken :: Word8 -> Bool
isToken w = w >= 33 && w <= 126

-- | Parse a number
aNumber :: AP.Parser Int
aNumber = read . C8.unpack <$> AP.takeWhile1 isNumber
  where
    isNumber :: Word8 -> Bool
    isNumber w = w >= 48 && w <= 57

-- | Check if a value can be in-/decremented
canInDecr :: Value -> Bool
canInDecr = isInteger . view value

-- | Check if a value is a valid integer, and thus eligeble for incr/decr
isInteger :: BS.ByteString -> Bool
isInteger = BS.all (`BS.elem` "1234567890")

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

