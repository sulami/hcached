{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State

import           Control.Lens
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
    it "can insert a key-value-pair" $ do
      let hm = insert' "1" "one" $ initialState False 10
      query' "1" hm `shouldBe` (Just "one")
    it "recognizes non-existent keys" $ do
      let hm = insert' "1" "one" $ initialState False 10
      query' "2" hm `shouldBe` Nothing
    it "only saves the specified amount of KVPs" $ do
      let hm = insert' "2" "two" $ insert' "1" "one" $ initialState False 1
      HML.size (hm^.hashMap) `shouldBe` 1
    it "deletes the first inserted key when full" $ do
      let hm = insert' "2" "two" $ insert' "1" "one" $ initialState False 1
      query' "1" hm `shouldBe` Nothing
      query' "2" hm `shouldBe` (Just "two")
    it "deletes the deleted key from the mru list" $ do
      let hm = insert' "2" "two" $ insert' "1" "one" $ initialState False 1
      hm^.mru `shouldBe` ["2"]
    it "updates the most recently used list to reflect queries" $ do
      let hm = insert' "2" "two" $ insert' "1" "one" $ initialState False 2
      (queried' "1" hm)^.mru `shouldBe` ["1", "2"]

