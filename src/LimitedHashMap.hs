{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)

import           Control.Lens ((^.), (%~), makeLenses)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as HML

-- | Stateful data needed for the size-limited hashtable
data LimitedHashMap = LimitedHashMap
  { _hashMap :: !(HML.HashMap ByteString ByteString) -- ^ The Hashmap used
  , _maxSize :: !Int                                 -- ^ Maximum hashmap size
  , _mru     :: ![ByteString]                        -- ^ Recently used hashes
  , _debug   :: !Bool                                -- ^ Debug output?
  }

makeLenses ''LimitedHashMap

-- | The inital state to use when starting up
initialState :: Bool -> Int -> LimitedHashMap
initialState dbg msize = LimitedHashMap HML.empty msize [] dbg

-- | Insert a new KVP
insert :: MVar LimitedHashMap -> ByteString -> ByteString -> IO ()
insert lhm k v = modifyMVar_ lhm $ insert' k v

-- | Perform the actual insertion
insert' :: ByteString -> ByteString -> LimitedHashMap -> IO LimitedHashMap
insert' k v s =
  let currentSize = HML.size $ s^.hashMap
      isFull = currentSize == s^.maxSize
      alreadyMember = HML.member k $ s^.hashMap
      needsDeletion = isFull && not alreadyMember
      delCandidate = head $ s^.mru
  in return $ hashMap %~ (HML.insert k v) $
     (if needsDeletion then mru %~ tail else id) $
     (if needsDeletion then hashMap %~ (HML.delete delCandidate) else id) $
     (mru %~ (++ [k])) s

-- | Query a value for a key
query :: MVar LimitedHashMap -> ByteString -> IO (Maybe ByteString)
query lhm k = do
  state <- readMVar lhm
  modifyMVar_ lhm $ queried' k
  return $ query' k state

-- | Pure version of 'query' for testing
query' :: ByteString -> LimitedHashMap -> Maybe ByteString
query' k s = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
queried' :: ByteString -> LimitedHashMap -> IO LimitedHashMap
queried' k lhm = return $ mru %~ ((k :) . (filter (/= k))) $ lhm

