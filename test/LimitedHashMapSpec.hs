{-# LANGUAGE OverloadedStrings #-}

module LimitedHashMapSpec where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad (when)
import           Data.Maybe (isJust, isNothing)

import           Control.Lens ((^.))
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

    it "can set a key-value-pair" $ do
      set mlhm "1" "one" 10
      get mlhm "1" `shouldReturn` Just "one"

    -- it "recognizes non-existent keys" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   hm <- readMVar lhm
    --   rv <- get lhm "2"
    --   when (isJust rv) $ assertFailure "Return nonexistent key"

    -- it "only saves the specified amount of KVPs" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   hm <- readMVar lhm
    --   HML.size (hm^.hashMap) `shouldBe` 1

    -- it "deletes the first set key when full" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   set lhm "2" "two" 10
    --   rv1 <- get lhm "1"
    --   rv1 `shouldBe` Nothing
    --   rv2 <- get lhm "2"
    --   case rv2 of
    --     Nothing  -> assertFailure "Empty result"
    --     Just val -> val^.value `shouldBe` "two"

    -- it "deletes the deleted key from the mru list" $ do
    --   lhm <- newMVar $ initialLHM 1
    --   set lhm "1" "one" 10
    --   set lhm "2" "two" 10
    --   hm <- readMVar lhm
    --   hm^.mru `shouldBe` ["2"]

    -- it "does not delete keys when replacing existing ones" $ do
    --   lhm <- newMVar $ initialLHM 2
    --   set lhm "1" "one" 10
    --   set lhm "2" "two" 10
    --   set lhm "1" "uno" 10
    --   rv2 <- get lhm "2"
    --   case rv2 of
    --     Nothing  -> assertFailure "Empty result"
    --     Just val -> val^.value `shouldBe` "two"
    --   rv1 <- get lhm "1"
    --   case rv1 of
    --     Nothing  -> assertFailure "Empty result"
    --     Just val -> val^.value `shouldBe` "uno"

    -- it "updates the most recently used list to reflect queries" $ do
    --   lhm <- newMVar $ initialLHM 2
    --   set lhm "1" "one" 10
    --   set lhm "2" "two" 10
    --   rv <- readMVar lhm >>= updateMRU "1"
    --   rv^.mru `shouldBe` ["1", "2"]

    -- it "sets the proper time-to-live" $ do
    --   lhm <- newMVar $ initialLHM 2
    --   set lhm "1" "one" 60
    --   now <- getPOSIXTime
    --   rv <- get lhm "1"
    --   case rv of
    --     Nothing  -> assertFailure "Empty result"
    --     Just val -> val^.ttl - now `shouldSatisfy` (> 55)

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

