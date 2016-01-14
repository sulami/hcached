{-# LANGUAGE OverloadedStrings #-}

module LimitedHashMapSpec where

import           Control.Arrow           ((&&&))
import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad           (liftM, when)
import           Data.Maybe              (isJust, isNothing)

import           Control.Lens            (view, (^.))
import           Data.ByteString         (ByteString)
import qualified Data.HashMap.Lazy       as HML
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Test.Hspec
import           Test.HUnit.Base         (assertFailure)

import           LimitedHashMap

-- | Point a fresh LHM before every test case
resetLHM :: MVar LimitedHashMap -> IO ()
resetLHM mv = do
  swapMVar mv $ initialLHM 2
  return ()

-- | Get the easily predictable parts of a value
testValue :: Maybe Value -> Maybe (ByteString, Int)
testValue = liftM (view value &&& view flags)

spec :: Spec
spec = do
  mlhm <- runIO . newMVar $ initialLHM 2
  before_ (resetLHM mlhm) $ do

    it "initializes to the proper state" $ do
      lhm <- readMVar mlhm
      lhm^.maxSize `shouldBe` 2
      lhm^.mru `shouldBe` []

    it "can set and get multiple key-value-pairs" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 7 10 "two"
      liftM testValue (get mlhm "1") `shouldReturn` Just ("one", 0)
      liftM testValue (get mlhm "2") `shouldReturn` Just ("two", 7)

    it "sets the proper time-to-live" $ do
      now <- getPOSIXTime
      set mlhm "1" 0 60 "one"
      set mlhm "2" 0 (now + 10) "two" -- Large values are considered unix time
      lhm <- readMVar mlhm
      case get' lhm "1" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 59 && t < 61)
      case get' lhm "2" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 9 && t < 11)

    it "can append and prepend values to existing values" $ do
      set mlhm "1" 0 60 "one"
      append mlhm "1" "toe"
      prepend mlhm "1" "ice"
      liftM testValue (get mlhm "1") `shouldReturn` Just ("iceonetoe", 0)

    it "can touch values" $ do
      set mlhm "1" 0 60 "one"
      now <- getPOSIXTime
      lhm <- readMVar mlhm
      case get' lhm "1" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 59 && t < 61)
      touch mlhm "1" 10
      lhm <- readMVar mlhm
      case get' lhm "1" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 9 && t < 11)

    it "recognizes non-existent keys" $
      liftM testValue (get mlhm "1") `shouldReturn` Nothing

    it "deletes the least recently set key when full" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "3" 0 10 "thr"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      liftM testValue (get mlhm "1") `shouldReturn` Nothing
      liftM testValue (get mlhm "2") `shouldReturn` Just ("two", 0)
      liftM testValue (get mlhm "3") `shouldReturn` Just ("thr", 0)

    it "deletes the least recently gotten key when full" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      get mlhm "1"
      set mlhm "3" 0 10 "thr"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      liftM testValue (get mlhm "1") `shouldReturn` Just ("one", 0)
      liftM testValue (get mlhm "2") `shouldReturn` Nothing
      liftM testValue (get mlhm "3") `shouldReturn` Just ("thr", 0)

    it "does not delete keys when updating existing ones" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "1" 0 10 "uno"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      liftM testValue (get mlhm "1") `shouldReturn` Just ("uno", 0)
      liftM testValue (get mlhm "2") `shouldReturn` Just ("two", 0)

    it "updates the most recently used list to reflect queries" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "1" 0 10 "uno"
      lhm <- readMVar mlhm
      lhm^.mru `shouldBe` ["2","1"]

    it "does not return expired KVPs" $ do
      set mlhm "1" 0 (-1) "one"
      liftM testValue (get mlhm "1") `shouldReturn` Nothing

    it "can delete a KVP from both the hashmap and the mru" $ do
      set mlhm "1" 0 10 "one"
      delete mlhm "1"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0
      liftM testValue (get mlhm "1") `shouldReturn` Nothing

    it "does not delete any KVP if the desired one does not exist" $ do
      set mlhm "1" 0 10 "one"
      delete mlhm "2"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 1
      length (lhm^.mru) `shouldBe` 1
      liftM testValue (get mlhm "1") `shouldReturn` Just ("one", 0)

    it "deletes expired KVPs when encountered" $ do
      set mlhm "1" 0 (-1) "one"
      liftM testValue (get mlhm "1") `shouldReturn` Nothing
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0

    it "cleans up expired KVPs when ordered to" $ do
      set mlhm "1" 0 (-1) "one"
      set mlhm "2" 0 10 "two"
      cleanup mlhm
      liftM testValue (get mlhm "2") `shouldReturn` Just ("two", 0)
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 1
      length (lhm^.mru) `shouldBe` 1

    it "flushes when ordered to" $ do
      set mlhm "2" 0 10 "two"
      flush mlhm 0
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0

    it "flushed only KVPs that are valid long enough" $ do
      set mlhm "1" 0 8 "one"
      set mlhm "2" 0 12 "two"
      flush mlhm 10
      isMember mlhm "1" `shouldReturn` True
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 1
      length (lhm^.mru) `shouldBe` 1

    it "increases the global counter when using a unique number" $ do
      old <- getUnique mlhm
      new <- getUnique mlhm
      new - old `shouldBe` 1

    it "increases the unique number when performing modifying opearations" $ do
      set mlhm "1" 0 8 "one"
      value <- get mlhm "1"
      lhm <- readMVar mlhm
      let value = get' lhm "1"
      case value of
        Nothing  -> assertFailure "Nothing returned"
        Just val -> do
          let old = val^.uniq
          updateUnique mlhm "1"
          lhm <- readMVar mlhm
          let value = get' lhm "1"
          case value of
            Nothing  -> assertFailure "Nothing returned"
            Just val -> val^.uniq - old `shouldBe` 1

    it "can check whether a value is a valid integer" $ do
      set mlhm "1" 0 8 "one"
      isInteger mlhm "1" `shouldReturn` Just False
      set mlhm "2" 0 8 "1"
      isInteger mlhm "2" `shouldReturn` Just True
      isInteger mlhm "3" `shouldReturn` Nothing

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

