{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import           Control.Concurrent.MVar (newMVar)
import           Data.Either (isLeft)

import           Test.Hspec

import           Command (Command (..), executeCommand, parse)
import           LimitedHashMap (flush, initialLHM)

spec :: Spec
spec = do
  describe "Command Parser" $ do
    it "parses basic valid commands" $ do
      parse "set key 23 1 11\nvalue value\n"
        `shouldBe` (Right $ SetCmd "key" 23 1 False "value value")
      parse "add key 24 0 5\nvalue\n"
        `shouldBe` (Right $ AddCmd "key" 24 0 False "value")
      parse "replace key 24 0 5\nvalue\n"
        `shouldBe` (Right $ ReplaceCmd "key" 24 0 False "value")
      parse "append key 5\nvalue\n"
        `shouldBe` (Right $ AppendCmd "key" False "value")
      parse "get key koy\n" `shouldBe` (Right $ GetCmd ["key", "koy"])
      parse "gets key\n" `shouldBe` (Right $ GetCmd ["key"])
      parse "delete key\n" `shouldBe` (Right $ DelCmd "key" False)
      parse "flush_all\n" `shouldBe` (Right $ FlushCmd 0 False)
      parse "flush_all 30 noreply\n" `shouldBe` (Right $ FlushCmd 30 True)

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

  describe "Command Executer" $ before_ (flush lhm 0) $ do
    it "correctly answers to set commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 False "val") `shouldReturn` "STORED"
      executeCommand lhm (SetCmd "keys" 0 10 True "val") `shouldReturn` ""

    it "correctly answers to add commands" $ do
      executeCommand lhm (AddCmd "kay" 0 10 False "val") `shouldReturn` "STORED"
      executeCommand lhm (AddCmd "kay" 0 10 False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand lhm (AddCmd "kay" 0 10 True "val")
        `shouldReturn` ""

    it "correctly answers to replace commands" $ do
      executeCommand lhm (ReplaceCmd "koy" 0 10 False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand lhm (AddCmd "koy" 0 10 False "val")
      executeCommand lhm (ReplaceCmd "koy" 0 10 False "val")
        `shouldReturn` "STORED"
      executeCommand lhm (ReplaceCmd "koy" 0 10 True "val")
        `shouldReturn` ""

    it "correctly answers to append commands" $ do
      executeCommand lhm (AppendCmd "key" False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand lhm (SetCmd "key" 0 10 True "val")
      executeCommand lhm (AppendCmd "key" False "val")
        `shouldReturn` "STORED"
      executeCommand lhm (GetCmd ["key"])
        `shouldReturn` "VALUE key 0 6\r\nvalval\r\nEND"

    it "correctly answers to get(s) commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 True "val")
      executeCommand lhm (SetCmd "keys" 0 10 True "val")
      executeCommand lhm (GetCmd ["no"]) `shouldReturn` "END"
      executeCommand lhm (GetCmd ["key"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nEND"
      executeCommand lhm (GetCmd ["key", "no", "keys"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nVALUE keys 0 3\r\nval\r\nEND"

    it "correctly answers to delete commands" $ do
      executeCommand lhm (SetCmd "key" 0 10 True "val")
      executeCommand lhm (DelCmd "key" False) `shouldReturn` "DELETED"
      executeCommand lhm (DelCmd "key" False) `shouldReturn` "NOT_FOUND"
      executeCommand lhm (DelCmd "key" True) `shouldReturn` ""

    it "correctly answers to flush commands" $ do
      executeCommand lhm (FlushCmd 0 False) `shouldReturn` "OK"
      executeCommand lhm (FlushCmd 5 True) `shouldReturn` ""

