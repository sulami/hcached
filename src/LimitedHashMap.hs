{-# LANGUAGE TemplateHaskell #-}

module LimitedHashMap where

import           Control.Lens ((^.), (%~), makeLenses)
import           Control.Monad.State (State, get, modify)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as HML

-- | Stateful data needed for the size-limited hashtable
data LimitedHashMap = LimitedHashMap
  { _hashMap :: !(HML.HashMap ByteString ByteString) -- ^ The Hashmap used
  , _maxSize :: !Int                                 -- ^ Maximum hashmap size
  , _used    :: ![ByteString]                        -- ^ Recently used hashes
  }

makeLenses ''LimitedHashMap

-- | The state monad for easy use of the LHM
type LHM a = State LimitedHashMap a

-- | The inital state to use when starting up
initialState :: Int -> LimitedHashMap
initialState msize = LimitedHashMap HML.empty msize []

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
      delCandidate = head $ s^.used
  in hashMap %~ (HML.insert k v) $
     (if needsDeletion then used %~ tail else id) $
     (if needsDeletion then hashMap %~ (HML.delete delCandidate) else id) $
     (used %~ (++ [k])) s

-- | Query a value for a key
query :: ByteString -> LHM (Maybe ByteString)
query k = do
  state <- get
  return $ query' k state

-- | Pure version of 'query' for testing
query' :: ByteString -> LimitedHashMap -> Maybe ByteString
query' k s = HML.lookup k $ s^.hashMap

