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
spec = describe "LimitedHashMap" $ do
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

    -- it "does not return expired KVPs" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" (-1)
    --   rv <- get lhm "1"
    --   when (isJust rv) $ assertFailure "Expired value returned"

    -- it "can delete a KVP from both the hashmap and the mru" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 0
    --   hm0 <- readMVar lhm
    --   HML.size (hm0^.hashMap) `shouldBe` 1
    --   length (hm0^.mru) `shouldBe` 1
    --   delete lhm "1"
    --   hm1 <- readMVar lhm
    --   HML.size (hm1^.hashMap) `shouldBe` 0
    --   length (hm1^.mru) `shouldBe` 0
    --   rv <- get lhm "1"
    --   when (isJust rv) $ assertFailure "Deleted value returned"

    -- it "deletes expired KVPs when encountered" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" (-1)
    --   hm0 <- readMVar lhm
    --   HML.size (hm0^.hashMap) `shouldBe` 1
    --   length (hm0^.mru) `shouldBe` 1
    --   rv <- get lhm "1"
    --   when (isJust rv) $ assertFailure "Expired value returned"
    --   hm1 <- readMVar lhm
    --   HML.size (hm1^.hashMap) `shouldBe` 0
    --   length (hm1^.mru) `shouldBe` 0

    -- it "deletes a KVP when ordered to" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   hm0 <- readMVar lhm
    --   HML.size (hm0^.hashMap) `shouldBe` 1
    --   length (hm0^.mru) `shouldBe` 1
    --   delete lhm "1"
    --   rv <- get lhm "1"
    --   when (isJust rv) $ assertFailure "Deleted value returned"
    --   hm1 <- readMVar lhm
    --   HML.size (hm1^.hashMap) `shouldBe` 0
    --   length (hm1^.mru) `shouldBe` 0

    -- it "does not delete any KVP if the desired one does not exist" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   hm0 <- readMVar lhm
    --   HML.size (hm0^.hashMap) `shouldBe` 1
    --   length (hm0^.mru) `shouldBe` 1
    --   delete lhm "2"
    --   rv <- get lhm "1"
    --   when (isNothing rv) $ assertFailure "No value returned"
    --   hm1 <- readMVar lhm
    --   HML.size (hm1^.hashMap) `shouldBe` 1
    --   length (hm1^.mru) `shouldBe` 1

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

