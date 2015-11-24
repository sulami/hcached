{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import           Control.Concurrent.MVar (newMVar)
import           Data.Either (isLeft)

import           Test.Hspec

import           Command (Command (..), executeCommand, parse)
import           LimitedHashMap (initialLHM)

spec :: Spec
spec = do
  describe "Command Parser" $ do
    it "parses basic valid commands" $ do
      parse "set key 23 1 11\nvalue value\n"
        `shouldBe` (Right $ SetCmd "key" 23 1 False "value value")
      parse "get key\n" `shouldBe` (Right $ GetCmd ["key"])
      parse "gets key\n" `shouldBe` (Right $ GetCmd ["key"])
      parse "delete key\n" `shouldBe` (Right $ DelCmd "key")

    it "parses set requests with multi-word values" $
      parse "set key 0 1 17\nvalue more values\n"
        `shouldBe` (Right $ SetCmd "key" 0 1 False "value more values")

    it "parses get(s) requests with several keys" $ do
      parse "get key1 key2\n" `shouldBe` (Right $ GetCmd ["key1","key2"])
      parse "gets key1 key2\n" `shouldBe` (Right $ GetCmd ["key1","key2"])

    it "parses the noreply keyword" $
      parse "set key 23 1 11 noreply\nvalue value\n"
        `shouldBe` (Right $ SetCmd "key" 23 1 True "value value")

    it "parses special characters in keys and values" $
      parse "set th!s-Key 0 1 12\nS_x$#%@^&{}\"\n"
        `shouldBe` (Right $ SetCmd "th!s-Key" 0 1 False "S_x$#%@^&{}\"")

    it "does not parse invalid content sizes" $ do
      parse "set key 0 1 4\nvalue\n" `shouldSatisfy` isLeft
      parse "set key 0 1 6\nvalue\n" `shouldSatisfy` isLeft

    it "does not parse empty requests" $
      parse "\n" `shouldSatisfy` isLeft

    it "does not parse requests without terminating newlines" $
      parse "get something" `shouldSatisfy` isLeft

    it "does not parse set commands with non-numerical numbers" $ do
      parse "set key 0 0xABC\nvalue\n" `shouldSatisfy` isLeft
      parse "set key 0xABC 0\nvalue\n" `shouldSatisfy` isLeft

  lhm <- runIO . newMVar $ initialLHM 3

  describe "Command Executer" $ do
    it "correctly answers to set commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 False "val") `shouldReturn` "STORED"
      executeCommand lhm (SetCmd "keys" 0 10 True "val") `shouldReturn` ""

    it "correctly answers to get(s) commands" $ do
      executeCommand lhm (GetCmd ["no"]) `shouldReturn` "END"
      executeCommand lhm (GetCmd ["key"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nEND"
      executeCommand lhm (GetCmd ["key", "no", "keys"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nVALUE keys 0 3\r\nval\r\nEND"

    it "correctly answers to delete commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 True "val")
      executeCommand lhm (DelCmd "key") `shouldReturn` "DELETED"
      executeCommand lhm (DelCmd "key") `shouldReturn` "NOT_FOUND"

