{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Arrow ((&&&))
import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import           Control.Monad (forM_, when)

import           Control.Lens ((^.), (%~), (.~), makeLenses, view)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Lazy as HML
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

-- | Data that is stored together with a value
data Value = Value
  { _value :: !ByteString -- ^ The actual value as supplied
  , _flags :: !Int        -- ^ Arbitrary flags
  , _ttl   :: !POSIXTime  -- ^ The time to live of this KVP
  } deriving (Show)

makeLenses ''Value

instance Eq Value where
  x == y = x^.value == y^.value

-- | Stateful data needed for the size-limited hashtable
data LimitedHashMap = LimitedHashMap
  { _hashMap :: !(HML.HashMap ByteString Value) -- ^ The Hashmap used
  , _maxSize :: !Int                            -- ^ Maximum hashmap size
  , _mru     :: ![ByteString]                   -- ^ Recently used hashes
  }

makeLenses ''LimitedHashMap

-- | The inital state to use when starting up
initialLHM :: Int -> LimitedHashMap
initialLHM msize = LimitedHashMap HML.empty msize []

-- | Insert a new KVP
set :: MVar LimitedHashMap -> ByteString -> Int -> POSIXTime -> ByteString
    -> IO ()
set lhm k f t v = do
  now <- getPOSIXTime
  let value | t >= 60*60*24*30 = Value v f t
            | otherwise        = Value v f $ now + t
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
append :: MVar LimitedHashMap -> ByteString -> ByteString -> IO ()
append lhm k v = do
  modifyMVar_ lhm $ updateMRU k
  modifyMVar_ lhm $
    return . (hashMap %~ HML.adjust (value %~ (`C8.append` v)) k)

-- | Query a value for a key
get :: MVar LimitedHashMap -> ByteString -> IO (Maybe (Int, ByteString))
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
        return $ Just . (view flags &&& view value) =<< rv

-- | Pure version of get for testing
get' :: LimitedHashMap -> ByteString -> Maybe Value
get' s k = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
updateMRU :: ByteString -> LimitedHashMap -> IO LimitedHashMap
updateMRU k lhm = return $ mru %~ (++ [k]) . filter (/= k) $ lhm

-- | Check if a key is part of the LHM without updating the MRU like get would
isMember :: MVar LimitedHashMap -> ByteString -> IO Bool
isMember lhm k = do
  l <- readMVar lhm
  return . HML.member k $ view hashMap l

-- | Delete a KVP
delete :: MVar LimitedHashMap -> ByteString -> IO ()
delete lhm k = do
  modifyMVar_ lhm (return . (hashMap %~ HML.delete k))
  modifyMVar_ lhm (return . (mru %~ filter (/= k)))

-- | Remove all expired KVPs from the LHM
cleanup :: MVar LimitedHashMap -> IO ()
cleanup lhm = do
  s <- readMVar lhm
  now <- getPOSIXTime
  let isExpired k v = now > v^.ttl
      toBeDeleted = HML.keys . HML.filterWithKey isExpired $ view hashMap s
  forM_ toBeDeleted $ delete lhm

-- | Flush out all KVPs that are valid as least as long as the specified time.
-- Short times are relative, long ones absolute, like when setting keys. A time
-- of zero empties the whole LHM
flush :: MVar LimitedHashMap -> POSIXTime -> IO ()
flush lhm 0 = modifyMVar_ lhm $ return . (hashMap .~ HML.empty) . (mru .~ [])
flush lhm t = do
  s <- readMVar lhm
  now <- getPOSIXTime
  let time | t >= 60*60*24*30 = t
           | otherwise        = now + t
      isToBeFlushed k v = time <= v^.ttl
      toBeFlushed = HML.keys . HML.filterWithKey isToBeFlushed $ view hashMap s
  forM_ toBeFlushed $ delete lhm

