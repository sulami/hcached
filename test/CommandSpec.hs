{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import           Control.Concurrent.MVar (newMVar)
import           Data.Either (isLeft)

import           Test.Hspec

import           Command (Command (..), executeCommand, parse)
import           LimitedHashMap (initialState)

spec :: Spec
spec = do
  describe "Command Parser" $ do
    it "parses basic valid commands" $ do
      parse "set 1 key value\n" `shouldBe` (Right $ SetCmd 1 "key" "value")
      parse "get key\n" `shouldBe` (Right $ GetCmd "key")
      parse "delete key\n" `shouldBe` (Right $ DelCmd "key")

    it "parses set requests with multi-word values" $
      parse "set 1 key value more values\n"
        `shouldBe` (Right $ SetCmd 1 "key" "value more values")

    it "does not parse empty requests" $
      parse "\n" `shouldSatisfy` isLeft

    it "does not parse requests without terminating newlines" $
      parse "get something" `shouldSatisfy` isLeft

    it "does not parse single-argument-commands with extraneous arguments" $ do
      parse "get key extra\n" `shouldSatisfy` isLeft
      parse "delete key extra\n" `shouldSatisfy` isLeft

    it "does not parse set commands with non-numerical TTLs" $
      parse "set 0xABC key value\n" `shouldSatisfy` isLeft

  lhm <- runIO . newMVar $ initialState False 3

  describe "Command Executer" $
    it "correctly executes commands and answers properly" $ do
      executeCommand lhm (SetCmd 10 "key" "val") `shouldReturn` "STORED"
      executeCommand lhm (GetCmd "key")          `shouldReturn` "VALUE val"
      executeCommand lhm (DelCmd "key")          `shouldReturn` "DELETED"
      executeCommand lhm (GetCmd "key")          `shouldReturn` "NOT_FOUND"
      executeCommand lhm (DelCmd "key")          `shouldReturn` "NOT_FOUND"

