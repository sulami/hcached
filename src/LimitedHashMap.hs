{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import           Control.Monad (forM_, when)

import           Control.Lens ((^.), (%~), makeLenses, view)
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
  }

makeLenses ''LimitedHashMap

-- | The inital state to use when starting up
initialLHM :: Int -> LimitedHashMap
initialLHM msize = LimitedHashMap HML.empty msize []

-- | Insert a new KVP
set :: MVar LimitedHashMap -> ByteString -> ByteString -> POSIXTime -> IO ()
set lhm k v t = do
  now <- getPOSIXTime
  let value | t >= 60*60*24*30 = Value v t
            | otherwise        = Value v $ now + t
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

-- | Query a value for a key
get :: MVar LimitedHashMap -> ByteString -> IO (Maybe ByteString)
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
        return $ Just . view value =<< rv

-- | Pure version of get for testing
get' :: LimitedHashMap -> ByteString -> Maybe Value
get' s k = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
updateMRU :: ByteString -> LimitedHashMap -> IO LimitedHashMap
updateMRU k lhm = return $ mru %~ (++ [k]) . filter (/= k) $ lhm

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

