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

    it "can set and get a key-value-pair" $ do
      set mlhm "1" "one" 10
      get mlhm "1" `shouldReturn` Just "one"

    it "can set and get multiple key-value-pairs" $ do
      set mlhm "1" "one" 10
      set mlhm "2" "two" 10
      get mlhm "1" `shouldReturn` Just "one"
      get mlhm "2" `shouldReturn` Just "two"

    it "sets the proper time-to-live" $ do
      set mlhm "1" "one" 60
      now <- getPOSIXTime
      lhm <- readMVar mlhm
      case get' "1" lhm of
        Nothing  -> assertFailure "Did not find deposited value"
        Just val -> val^.ttl - now `shouldSatisfy` (\t -> t > 59 && t <= 60)

    it "recognizes non-existent keys" $
      get mlhm "1" `shouldReturn` Nothing

    it "deletes the least recently set key when full" $ do
      set mlhm "1" "one" 10
      set mlhm "2" "two" 10
      set mlhm "3" "thr" 10
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Nothing
      get mlhm "2" `shouldReturn` Just "two"
      get mlhm "3" `shouldReturn` Just "thr"

    it "deletes the least recently gotten key when full" $ do
      set mlhm "1" "one" 10
      set mlhm "2" "two" 10
      get mlhm "1"
      set mlhm "3" "thr" 10
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Just "one"
      get mlhm "2" `shouldReturn` Nothing
      get mlhm "3" `shouldReturn` Just "thr"

    it "does not delete keys when updating existing ones" $ do
      set mlhm "1" "one" 10
      set mlhm "2" "two" 10
      set mlhm "1" "uno" 10
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 2
      length (lhm^.mru) `shouldBe` 2
      get mlhm "1" `shouldReturn` Just "uno"
      get mlhm "2" `shouldReturn` Just "two"

    it "updates the most recently used list to reflect queries" $ do
      set mlhm "1" "one" 10
      set mlhm "2" "two" 10
      set mlhm "1" "uno" 10
      lhm <- readMVar mlhm
      lhm^.mru `shouldBe` ["2","1"]

    it "does not return expired KVPs" $ do
      set mlhm "1" "one" (-1)
      get mlhm "1" `shouldReturn` Nothing

    it "can delete a KVP from both the hashmap and the mru" $ do
      set mlhm "1" "one" 10
      delete mlhm "1"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0
      get mlhm "1" `shouldReturn` Nothing

    it "does not delete any KVP if the desired one does not exist" $ do
      set mlhm "1" "one" 10
      delete mlhm "2"
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 1
      length (lhm^.mru) `shouldBe` 1
      get mlhm "1" `shouldReturn` Just "one"

    it "deletes expired KVPs when encountered" $ do
      set mlhm "1" "one" (-1)
      get mlhm "1" `shouldReturn` Nothing
      lhm <- readMVar mlhm
      HML.size (lhm^.hashMap) `shouldBe` 0
      length (lhm^.mru) `shouldBe` 0

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

