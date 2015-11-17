{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)

import           Control.Lens ((^.), (%~), makeLenses)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as HML
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

-- | Data that is stored together with a value
data Value = Value
  { _value :: !ByteString -- ^ The actual value as supplied
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
  , _debug   :: !Bool                           -- ^ Debug output?
  }

makeLenses ''LimitedHashMap

-- | The inital state to use when starting up
initialState :: Bool -> Int -> LimitedHashMap
initialState dbg msize = LimitedHashMap HML.empty msize [] dbg

-- | Insert a new KVP
set :: MVar LimitedHashMap -> ByteString -> ByteString -> POSIXTime -> IO ()
set lhm k v t = modifyMVar_ lhm $ set' k v t

-- | Perform the actual insertion
set' :: ByteString -> ByteString -> POSIXTime -> LimitedHashMap
     -> IO LimitedHashMap
set' k v t s =
  let currentSize = HML.size $ s^.hashMap
      -- TODO delete expired keys to make space if needed
      isFull = currentSize == s^.maxSize
      alreadyMember = HML.member k $ s^.hashMap
      needsDeletion = isFull && not alreadyMember
      delCandidate = head $ s^.mru
  in do
    value <- buildValue v t
    return $ hashMap %~ (HML.insert k value) $
     (if needsDeletion then mru %~ tail else id) $
     (if needsDeletion then hashMap %~ (HML.delete delCandidate) else id) $
     (mru %~ (++ [k])) s

-- | Construct a 'Value' and calculate its TTL
buildValue :: ByteString -> POSIXTime -> IO Value
buildValue val t = do
  now <- getPOSIXTime
  return $ Value val (now + t)

-- | Query a value for a key
get :: MVar LimitedHashMap -> ByteString -> IO (Maybe Value)
get lhm k = do
  state <- readMVar lhm
  now <- getPOSIXTime
  case get' k state of
    Nothing  -> return Nothing
    Just val -> if val^.ttl < now
      then do
        delete lhm k
        return Nothing
      else do
        modifyMVar_ lhm $ updateMRU k
        return $ get' k state

-- | Pure version of 'query' for testing
get' :: ByteString -> LimitedHashMap -> Maybe Value
get' k s = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
updateMRU :: ByteString -> LimitedHashMap -> IO LimitedHashMap
updateMRU k lhm = return $ mru %~ ((k :) . (filter (/= k))) $ lhm

-- | Delete a KVP
delete :: MVar LimitedHashMap -> ByteString -> IO ()
delete lhm k = do
  modifyMVar_ lhm (return . (hashMap %~ (HML.delete k)))
  modifyMVar_ lhm (return . (mru %~ (filter (/= k))))

