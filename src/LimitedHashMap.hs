{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Arrow           ((&&&))
import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import           Control.Monad           (forM_, liftM, when, (>=>))
import           Data.Maybe              (fromJust)
import           Data.Word               (Word64)

import           Control.Lens            (makeLenses, over, view, (%~), (.~),
                                          (^.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8
import qualified Data.HashMap.Lazy       as HML
import           Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime)

-- | Data that is stored together with a value
data Value = Value
  { _value :: !ByteString -- ^ The actual value as supplied
  , _flags :: !Int        -- ^ Arbitrary flags
  , _ttl   :: !POSIXTime  -- ^ The time to live of this KVP
  , _uniq  :: !Integer    -- ^ Monotonically increasing unique value
  } deriving (Show)

makeLenses ''Value

instance Eq Value where
  x == y = x^.value == y^.value

-- | The type used as unique identifier for KVPs
type Key = ByteString

-- | Stateful data needed for the size-limited hashtable
data LimitedHashMap = LimitedHashMap
  { _hashMap :: !(HML.HashMap Key Value) -- ^ The Hashmap used
  , _maxSize :: !Int                            -- ^ Maximum hashmap size
  , _mru     :: ![ByteString]                   -- ^ Recently used hashes
  , _counter :: !Integer                        -- ^ Insertion counter
  }

makeLenses ''LimitedHashMap

-- | The inital state to use when starting up
initialLHM :: Int -> LimitedHashMap
initialLHM msize = LimitedHashMap HML.empty msize [] 0

-- | Insert a new KVP
set :: MVar LimitedHashMap -> Key -> Int -> POSIXTime -> ByteString -> IO ()
set lhm k f t v = do
  time <- convertTime t
  unique <- getUnique lhm
  let value = Value v f time unique
  modifyMVar_ lhm $ \s -> do
    let isFull = HML.size (s^.hashMap) >= s^.maxSize
        alreadyMember = HML.member k $ s^.hashMap
        needsDeletion = isFull && not alreadyMember
        delCandidate = head $ s^.mru
        addToMRU = if alreadyMember
          then mru %~ ((++ [k]) . filter (/= k))
          else mru %~ (++ [k])
        performDeletion = if needsDeletion
          then (mru %~ tail) . (hashMap %~ HML.delete delCandidate)
          else id
    return $ hashMap %~ HML.insert k value $ performDeletion $ addToMRU s

-- | Append a value to an existing value
append :: MVar LimitedHashMap -> Key -> ByteString -> IO ()
append lhm k v = do
  updateUnique lhm k
  modifyMVar_ lhm $ updateMRU k >=>
    return . (hashMap %~ HML.adjust (value %~ (`C8.append` v)) k)

-- | Prepend a value to an existing value
prepend :: MVar LimitedHashMap -> Key -> ByteString -> IO ()
prepend lhm k v = do
  updateUnique lhm k
  modifyMVar_ lhm $ updateMRU k >=>
    return . (hashMap %~ HML.adjust (value %~ C8.append v) k)

-- | Query a value for a key
get :: MVar LimitedHashMap -> Key -> IO (Maybe Value)
get lhm k = do
  state <- readMVar lhm
  now <- getPOSIXTime
  let rv = get' state k
  case rv of
    Nothing  -> return Nothing
    Just val -> if val^.ttl < now
      then do
        delete lhm k
        return Nothing
      else do
        modifyMVar_ lhm $ updateMRU k
        return rv

-- | Pure version of get for testing
get' :: LimitedHashMap -> Key -> Maybe Value
get' s k = HML.lookup k $ s^.hashMap

-- | Check if a key is part of the LHM without updating the MRU like get would
isMember :: MVar LimitedHashMap -> Key -> IO Bool
isMember lhm k = do
  l <- readMVar lhm
  return . HML.member k $ view hashMap l

-- | Delete a KVP
delete :: MVar LimitedHashMap -> Key -> IO ()
delete lhm k = modifyMVar_ lhm $
  return . (hashMap %~ HML.delete k) . (mru %~ filter (/= k))

-- | Perform an incr command and return the new value
incr :: MVar LimitedHashMap -> Key -> Word64 -> IO ByteString
incr lhm k n = do
  modifyMVar_ lhm $ return . (hashMap %~ HML.adjust (doIncr n) k)
  liftM (view value . fromJust) $ get lhm k

-- | Increment a value by n, wrapping at the unsigned 64-bit mark
doIncr :: Word64 -> Value -> Value
doIncr n = over value increment
  where
    increment :: C8.ByteString -> C8.ByteString
    increment bs = let num = read $ C8.unpack bs :: Word64
                    in C8.pack . show $ num + n

-- | Perform a decr command and return the new value
decr :: MVar LimitedHashMap -> Key -> Word64 -> IO ByteString
decr lhm k n = do
  modifyMVar_ lhm $ return . (hashMap %~ HML.adjust (doDecr n) k)
  liftM (view value . fromJust) $ get lhm k

-- | Decrement a value by n, stopping at zero
doDecr :: Word64 -> Value -> Value
doDecr n = over value decrement
  where
    decrement :: C8.ByteString -> C8.ByteString
    decrement bs = let num = read $ C8.unpack bs :: Word64
                       new = num - n :: Word64
                       repr = C8.pack . show
                    in if new < num then repr new else repr 0

-- | Remove all expired KVPs from the LHM
cleanup :: MVar LimitedHashMap -> IO ()
cleanup lhm = do
  s <- readMVar lhm
  now <- getPOSIXTime
  let isExpired k v = now > v^.ttl
      toBeDeleted = HML.keys . HML.filterWithKey isExpired $ view hashMap s
  forM_ toBeDeleted $ delete lhm

-- | Update just the TTL of a KVP
touch :: MVar LimitedHashMap -> Key -> POSIXTime -> IO ()
touch lhm k t = do
  time <- convertTime t
  updateUnique lhm k
  modifyMVar_ lhm $
    updateMRU k >=> return . (hashMap %~ HML.adjust (ttl .~ time) k)

-- | Flush out all KVPs that are valid as least as long as the specified time.
-- Short times are relative, long ones absolute, like when setting keys. A time
-- of zero empties the whole LHM
flush :: MVar LimitedHashMap -> POSIXTime -> IO ()
flush lhm 0 = modifyMVar_ lhm $ return . (hashMap .~ HML.empty) . (mru .~ [])
flush lhm t = do
  s <- readMVar lhm
  time <- convertTime t
  let isToBeFlushed k v = time <= v^.ttl
      toBeFlushed = HML.keys . HML.filterWithKey isToBeFlushed $ view hashMap s
  forM_ toBeFlushed $ delete lhm

-- | Check if a supplied time is relative or absolute and convert it if
-- neccessary
convertTime :: POSIXTime -> IO POSIXTime
convertTime t = do
  now <- getPOSIXTime
  return $ if t >= 60*60*24*30
    then t
    else now + t

-- | Update the most recently mru list to reflect a query
updateMRU :: Key -> LimitedHashMap -> IO LimitedHashMap
updateMRU k = return . (mru %~ (++ [k]) . filter (/= k))
{-# ANN updateMRU "HLint: ignore Redundant bracket" #-}

-- | Get a new unique number and assign it to a value
updateUnique :: MVar LimitedHashMap -> Key -> IO ()
updateUnique lhm k = do
  new <- getUnique lhm
  modifyMVar_ lhm $ return . (hashMap %~ HML.adjust (uniq .~ new) k)

-- | Get the unique number of a value, if it exists
viewUnique :: MVar LimitedHashMap -> Key -> IO (Maybe Integer)
viewUnique lhm k = do
  mlhm <- readMVar lhm
  let value = get' mlhm k
  case value of
    Nothing  -> return Nothing
    Just val -> return . Just $ val^.uniq

-- | Get a unique number and increment the insertion counter
getUnique :: MVar LimitedHashMap -> IO Integer
getUnique lhm = do
  unique <- view counter <$> readMVar lhm
  modifyMVar_ lhm $ return . (counter %~ (+1))
  return unique

