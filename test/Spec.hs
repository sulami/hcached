{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State

import           Control.Lens
import           Test.Hspec

import           LimitedHashMap

main :: IO ()
main = hspec $ do
  describe "LimitedHashMap" $ do
    it "sets the proper initial maximum size" $ do
      (initialState 10)^.maxSize `shouldBe` 10
    it "sets the proper initial used list" $ do
      (initialState 10)^.used `shouldBe` []
    it "can insert a key-value-pair" $ do
      (query' "1" $ insert' "1" "one" $ initialState 10) `shouldBe` (Just "one")
    it "recognizes non-existent keys" $ do
      (query' "2" $ insert' "1" "one" $ initialState 10) `shouldBe` Nothing
    it "should only save the specified amount of KVPs" $ do
      let hm = insert' "2" "two" $ insert' "1" "one" $ initialState 1
      query' "1" hm `shouldBe` Nothing
      query' "2" hm `shouldBe` (Just "two")

