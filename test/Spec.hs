{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^.))
import           Test.Hspec
import qualified Data.HashMap.Lazy as HML

import           LimitedHashMap

main :: IO ()
main = hspec $ do
  describe "LimitedHashMap" $ do
    it "sets the proper initial maximum size" $ do
      (initialState False 10)^.maxSize `shouldBe` 10
    it "sets the proper initial used list" $ do
      (initialState False 10)^.mru `shouldBe` []
    it "can set a key-value-pair" $ do
      hm <- set' "1" "one" $ initialState False 10
      get' "1" hm `shouldBe` (Just "one")
    it "recognizes non-existent keys" $ do
      hm <- set' "1" "one" $ initialState False 10
      get' "2" hm `shouldBe` Nothing
    it "only saves the specified amount of KVPs" $ do
      hm <- (set' "2" "two") =<< set' "1" "one" (initialState False 1)
      HML.size (hm^.hashMap) `shouldBe` 1
    it "deletes the first set key when full" $ do
      hm <- (set' "2" "two") =<< set' "1" "one" (initialState False 1)
      get' "1" hm `shouldBe` Nothing
      get' "2" hm `shouldBe` (Just "two")
    it "deletes the deleted key from the mru list" $ do
      hm <- (set' "2" "two") =<< set' "1" "one" (initialState False 1)
      hm^.mru `shouldBe` ["2"]
    it "updates the most recently used list to reflect queries" $ do
      hm <- (set' "2" "two") =<< set' "1" "one" (initialState False 2)
      rv <- updateMRU "1" hm
      rv^.mru `shouldBe` ["1", "2"]

