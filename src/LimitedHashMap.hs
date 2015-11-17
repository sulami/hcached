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
set :: MVar LimitedHashMap -> ByteString -> ByteString -> IO ()
set lhm k v = modifyMVar_ lhm $ set' k v

-- | Perform the actual insertion
set' :: ByteString -> ByteString -> LimitedHashMap -> IO LimitedHashMap
set' k v s =
  let currentSize = HML.size $ s^.hashMap
      isFull = currentSize == s^.maxSize
      alreadyMember = HML.member k $ s^.hashMap
      needsDeletion = isFull && not alreadyMember
      delCandidate = head $ s^.mru
  in do
    value <- buildValue v
    return $ hashMap %~ (HML.insert k value) $
     (if needsDeletion then mru %~ tail else id) $
     (if needsDeletion then hashMap %~ (HML.delete delCandidate) else id) $
     (mru %~ (++ [k])) s

buildValue :: ByteString -> IO Value
buildValue val = do
  now <- getPOSIXTime
  return $ Value val now

-- | Query a value for a key
get :: MVar LimitedHashMap -> ByteString -> IO (Maybe Value)
get lhm k = do
  state <- readMVar lhm
  modifyMVar_ lhm $ updateMRU k
  return $ get' k state

-- | Pure version of 'query' for testing
get' :: ByteString -> LimitedHashMap -> Maybe Value
get' k s = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
updateMRU :: ByteString -> LimitedHashMap -> IO LimitedHashMap
updateMRU k lhm = return $ mru %~ ((k :) . (filter (/= k))) $ lhm

