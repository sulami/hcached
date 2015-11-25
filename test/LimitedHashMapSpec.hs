{-# LANGUAGE OverloadedStrings #-}

module LimitedHashMapSpec where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad (when)
import           Data.Maybe (isJust, isNothing)

import           Control.Lens ((^.), view)
import qualified Data.HashMap.Lazy as HML
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Test.Hspec
import           Test.HUnit.Base (assertFailure)

import           LimitedHashMap

-- | Point a fresh LHM before every test case
resetLHM :: MVar LimitedHashMap -> IO ()
resetLHM mv = do
  swapMVar mv $ initialLHM 2
  return ()

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
      get mlhm "1" `shouldReturn` Just (0, "one")
      get mlhm "2" `shouldReturn` Just (7, "two")

    it "sets the proper time-to-live" $ do
      now <- getPOSIXTime
      set mlhm "1" 0 60 "one"
      set mlhm "2" 0 (now + 10) "two" -- Large values are considered unix time
      lhm <- readMVar mlhm
      case get' lhm "1" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 59 && t <= 60)
      case get' lhm "2" of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 9 && t <= 10)

    it "recognizes non-existent keys" $
      get mlhm "1" `shouldReturn` Nothing

    it "deletes the least recently set key when full" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "3" 0 10 "thr"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Nothing
      get mlhm "2" `shouldReturn` Just (0, "two")
      get mlhm "3" `shouldReturn` Just (0, "thr")

    it "deletes the least recently gotten key when full" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      get mlhm "1"
      set mlhm "3" 0 10 "thr"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Just (0, "one")
      get mlhm "2" `shouldReturn` Nothing
      get mlhm "3" `shouldReturn` Just (0, "thr")

    it "does not delete keys when updating existing ones" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "1" 0 10 "uno"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Just (0, "uno")
      get mlhm "2" `shouldReturn` Just (0, "two")

    it "updates the most recently used list to reflect queries" $ do
      set mlhm "1" 0 10 "one"
      set mlhm "2" 0 10 "two"
      set mlhm "1" 0 10 "uno"
      lhm <- readMVar mlhm
      lhm^.mru `shouldBe` ["2","1"]

    it "does not return expired KVPs" $ do
      set mlhm "1" 0 (-1) "one"
      get mlhm "1" `shouldReturn` Nothing

    it "can delete a KVP from both the hashmap and the mru" $ do
      set mlhm "1" 0 10 "one"
      delete mlhm "1"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0
      get mlhm "1" `shouldReturn` Nothing

    it "does not delete any KVP if the desired one does not exist" $ do
      set mlhm "1" 0 10 "one"
      delete mlhm "2"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 1
      length (lhm^.mru) `shouldBe` 1
      get mlhm "1" `shouldReturn` Just (0, "one")

    it "deletes expired KVPs when encountered" $ do
      set mlhm "1" 0 (-1) "one"
      get mlhm "1" `shouldReturn` Nothing
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0

    it "cleans up expired KVPs when ordered to" $ do
      set mlhm "1" 0 (-1) "one"
      set mlhm "2" 0 10 "two"
      cleanup mlhm
      get mlhm "2" `shouldReturn` Just (0, "two")
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

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

