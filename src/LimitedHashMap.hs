{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Lens ((^.), (%~), makeLenses)
import           Control.Monad.State (StateT, get, modify)
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

-- | The monad stack for easy use of the LHM
type LHM a = StateT LimitedHashMap IO a

-- | The inital state to use when starting up
initialState :: Bool -> Int -> LimitedHashMap
initialState dbg msize = LimitedHashMap HML.empty msize [] dbg

-- | Insert a new KVP
insert :: ByteString -> ByteString -> LHM ()
insert k v = modify $ insert' k v

-- | Pure version of 'insert' for testing
insert' :: ByteString -> ByteString -> LimitedHashMap -> LimitedHashMap
insert' k v s =
  let currentSize = HML.size $ s^.hashMap
      isFull = currentSize == s^.maxSize
      alreadyMember = HML.member k $ s^.hashMap
      needsDeletion = isFull && not alreadyMember
      delCandidate = head $ s^.mru
  in hashMap %~ (HML.insert k v) $
     (if needsDeletion then mru %~ tail else id) $
     (if needsDeletion then hashMap %~ (HML.delete delCandidate) else id) $
     (mru %~ (++ [k])) s

-- | Query a value for a key
query :: ByteString -> LHM (Maybe ByteString)
query k = do
  state <- get
  modify $ queried' k
  return $ query' k state

-- | Pure version of 'query' for testing
query' :: ByteString -> LimitedHashMap -> Maybe ByteString
query' k s = HML.lookup k $ s^.hashMap

-- | Update the most recently mru list to reflect a query
queried' :: ByteString -> LimitedHashMap -> LimitedHashMap
queried' k = mru %~ ((k :) . (filter (/= k)))

