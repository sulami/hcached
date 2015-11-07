{-# LANGUAGE TemplateHaskell #-}

module HashTable (
) where

import           Control.Lens ((^.), makeLenses)
import           Control.Monad.State (State, get)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as HML

-- | Stateful data needed for the size-limited hashtable
data LimitedHashMap = LimitedHashMap
  { _hashMap :: !(HML.HashMap ByteString ByteString) -- ^ The Hashmap used
  , _maxSize :: !Integer                             -- ^ Maximum hashmap size
  , _used    :: ![ByteString]                        -- ^ Recently used hashes
  }

makeLenses ''LimitedHashMap

-- | The state monad for easy use of the LHM
type LHM a = State LimitedHashMap a

-- | The inital state to use when starting up
initalState :: Integer -> LimitedHashMap
initalState msize = LimitedHashMap HML.empty msize []

