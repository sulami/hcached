{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.MVar (newMVar, readMVar)
import           Control.Monad (when)
import           Data.Maybe (isJust)

import           Control.Lens ((^.))
import qualified Data.HashMap.Lazy as HML
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Test.Hspec
import           Test.HUnit.Base (assertFailure)

import           LimitedHashMap

main :: IO ()
main = hspec $ do
  describe "LimitedHashMap" $ do
    it "sets the proper initial maximum size" $ do
      (initialState False 10)^.maxSize `shouldBe` 10

    it "sets the proper initial used list" $ do
      (initialState False 10)^.mru `shouldBe` []

    it "can set a key-value-pair" $ do
      hm <- set' "1" "one" 10 $ initialState False 10
      case get' "1" hm of
        Nothing  -> assertFailure "Empty result"
        Just val -> val^.value `shouldBe` "one"

    it "recognizes non-existent keys" $ do
      hm <- set' "1" "one" 0 $ initialState False 10
      get' "2" hm `shouldBe` Nothing

    it "only saves the specified amount of KVPs" $ do
      hm <- (set' "2" "two" 10) =<< set' "1" "one" 10 (initialState False 1)
      HML.size (hm^.hashMap) `shouldBe` 1

    it "deletes the first set key when full" $ do
      hm <- (set' "2" "two" 10) =<< set' "1" "one" 10 (initialState False 1)
      get' "1" hm `shouldBe` Nothing
      case get' "2" hm of
        Nothing  -> assertFailure "Empty result"
        Just val -> val^.value `shouldBe` "two"

    it "deletes the deleted key from the mru list" $ do
      hm <- (set' "2" "two" 10) =<< set' "1" "one" 10 (initialState False 1)
      hm^.mru `shouldBe` ["2"]

    it "does not delete keys when replacing existing ones" $ do
      hm <- (set' "1" "uno" 10) =<< (set' "2" "two" 10)
            =<< set' "1" "one" 10 (initialState False 2)
      case get' "2" hm of
        Nothing  -> assertFailure "Empty result"
        Just val -> val^.value `shouldBe` "two"
      case get' "1" hm of
        Nothing  -> assertFailure "Empty result"
        Just val -> val^.value `shouldBe` "uno"

    it "updates the most recently used list to reflect queries" $ do
      hm <- (set' "2" "two" 10) =<< set' "1" "one" 10 (initialState False 2)
      rv <- updateMRU "1" hm
      rv^.mru `shouldBe` ["1", "2"]

    it "sets the proper time-to-live" $ do
      hm <- set' "1" "one" 60 (initialState False 2)
      now <- getPOSIXTime
      case get' "1" hm of
        Nothing  -> assertFailure "Empty result"
        Just val -> val^.ttl - now `shouldBe` 60

    it "does not return expired KVPs" $ do
      lhm <- newMVar $ initialState False 1
      set lhm "1" "one" (-1)
      rv <- get lhm "1"
      when (isJust rv) $ assertFailure "Expired value returned"

