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
      parse "set key 23 1 11 value value\n"
        `shouldBe` (Right $ SetCmd "key" 23 1 "value value")
      parse "get key\n" `shouldBe` (Right $ GetCmd "key")
      parse "delete key\n" `shouldBe` (Right $ DelCmd "key")

    it "parses set requests with multi-word values" $
      parse "set key 0 1 17 value more values\n"
        `shouldBe` (Right $ SetCmd "key" 0 1 "value more values")

    it "parses special characters in keys and values" $
      parse "set th!s-Key 0 1 12 S_x$#%@^&{}\"\n"
        `shouldBe` (Right $ SetCmd "th!s-Key" 0 1 "S_x$#%@^&{}\"")

    it "does not parse invalid content sizes" $ do
      parse "set key 0 1 4 value\n" `shouldSatisfy` isLeft
      parse "set key 0 1 6 value\n" `shouldSatisfy` isLeft

    it "does not parse empty requests" $
      parse "\n" `shouldSatisfy` isLeft

    it "does not parse requests without terminating newlines" $
      parse "get something" `shouldSatisfy` isLeft

    it "does not parse single-argument-commands with extraneous arguments" $ do
      parse "get key extra\n" `shouldSatisfy` isLeft
      parse "delete key extra\n" `shouldSatisfy` isLeft

    it "does not parse set commands with non-numerical numbers" $ do
      parse "set key 0 0xABC value\n" `shouldSatisfy` isLeft
      parse "set key 0xABC 0 value\n" `shouldSatisfy` isLeft

  lhm <- runIO . newMVar $ initialLHM 3

  describe "Command Executer" $
    it "correctly answers to commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 "val") `shouldReturn` "STORED"
      executeCommand lhm (GetCmd "key")            `shouldReturn` "VALUE 0 val"
      executeCommand lhm (DelCmd "key")            `shouldReturn` "DELETED"
      executeCommand lhm (GetCmd "key")            `shouldReturn` "NOT_FOUND"
      executeCommand lhm (DelCmd "key")            `shouldReturn` "NOT_FOUND"

