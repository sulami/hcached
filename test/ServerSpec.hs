{-# LANGUAGE OverloadedStrings #-}

module ServerSpec where


import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (newMVar, readMVar)
import           Data.ByteString.Char8 (pack)
import           Data.Either (isLeft)
import           Data.Maybe (isJust)
import           Data.Version (showVersion)
import           System.IO (BufferMode (..), hGetLine, hPutStr, hSetBuffering)

import           Control.Lens ((^.), view)
import           Network (PortID (..), connectTo)
import           Test.Hspec

import           LimitedHashMap
import qualified Paths_hcached as P
import           Server

spec :: Spec
spec = do
  istate <- runIO $ initialState False 0 3

  describe "Exposed Interface" $ do
    runIO . forkIO $ runServer istate 11212

    it "answers to requests on its port" $ do
      handle <- connectTo "localhost" $ PortNumber 11212
      hSetBuffering handle LineBuffering
      hPutStr handle "get key\n"
      hGetLine handle `shouldReturn` "END\r"

  describe "Janitor" $ do
    mst <- runIO $ newMVar istate
    runIO . forkIO $ janitor mst

    it "cleans up regularly" $ do
      mlhm <- view lhm <$> readMVar mst
      set mlhm "one" 0 (-1) "1"
      hmBefore <- readMVar mlhm
      get' hmBefore "one" `shouldSatisfy` isJust
      threadDelay 1000
      hmAfter <- readMVar mlhm
      get' hmAfter "one" `shouldBe` Nothing

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
      parse "prepend key 5\nvalue\n"
        `shouldBe` (Right $ PrependCmd "key" False "value")
      parse "get key koy\n" `shouldBe` (Right $ GetCmd ["key", "koy"])
      parse "gets key\n" `shouldBe` (Right $ GetCmd ["key"])
      parse "delete key\n" `shouldBe` (Right $ DeleteCmd "key" False)
      parse "touch key 10\n" `shouldBe` (Right $ TouchCmd "key" 10 False)
      parse "flush_all\n" `shouldBe` (Right $ FlushCmd 0 False)
      parse "flush_all 30 noreply\n" `shouldBe` (Right $ FlushCmd 30 True)
      parse "version\n" `shouldBe` Right VersionCmd
      parse "quit\n" `shouldBe` Right QuitCmd

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

  describe "Command Executer" $ before_ (flush (istate^.lhm) 0) $ do
    ss <- runIO $ newMVar istate

    it "correctly answers to set commands" $ do
      executeCommand ss (SetCmd "key" 0 10 False "val") `shouldReturn` "STORED"
      executeCommand ss (SetCmd "keys" 0 10 True "val") `shouldReturn` ""

    it "correctly answers to add commands" $ do
      executeCommand ss (AddCmd "kay" 0 10 False "val") `shouldReturn` "STORED"
      executeCommand ss (AddCmd "kay" 0 10 False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand ss (AddCmd "kay" 0 10 True "val")
        `shouldReturn` ""

    it "correctly answers to replace commands" $ do
      executeCommand ss (ReplaceCmd "koy" 0 10 False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand ss (AddCmd "koy" 0 10 False "val")
      executeCommand ss (ReplaceCmd "koy" 0 10 False "val")
        `shouldReturn` "STORED"
      executeCommand ss (ReplaceCmd "koy" 0 10 True "val")
        `shouldReturn` ""

    it "correctly answers to append and prepend commands" $ do
      executeCommand ss (AppendCmd "key" False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand ss (PrependCmd "key" False "val")
        `shouldReturn` "NOT_STORED"
      executeCommand ss (SetCmd "key" 0 10 True "val")
      executeCommand ss (AppendCmd "key" False "val")
        `shouldReturn` "STORED"
      executeCommand ss (PrependCmd "key" False "val")
        `shouldReturn` "STORED"
      executeCommand ss (GetCmd ["key"])
        `shouldReturn` "VALUE key 0 9\r\nvalvalval\r\nEND"

    it "correctly answers to get(s) commands" $ do
      executeCommand ss (SetCmd "key" 0 10 True "val")
      executeCommand ss (SetCmd "keys" 0 10 True "val")
      executeCommand ss (GetCmd ["no"]) `shouldReturn` "END"
      executeCommand ss (GetCmd ["key"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nEND"
      executeCommand ss (GetCmd ["key", "no", "keys"])
        `shouldReturn` "VALUE key 0 3\r\nval\r\nVALUE keys 0 3\r\nval\r\nEND"

    it "correctly answers to delete commands" $ do
      executeCommand ss (SetCmd "key" 0 10 True "val")
      executeCommand ss (DeleteCmd "key" False) `shouldReturn` "DELETED"
      executeCommand ss (DeleteCmd "key" False) `shouldReturn` "NOT_FOUND"
      executeCommand ss (DeleteCmd "key" True) `shouldReturn` ""

    it "correctly answers to touch commands" $ do
      executeCommand ss (TouchCmd "key" 5 False) `shouldReturn` "NOT_FOUND"
      executeCommand ss (SetCmd "key" 0 10 True "val")
      executeCommand ss (TouchCmd "key" 5 False) `shouldReturn` "TOUCHED"

    it "correctly answers to flush commands" $ do
      executeCommand ss (FlushCmd 0 False) `shouldReturn` "OK"
      executeCommand ss (FlushCmd 5 True) `shouldReturn` ""

    it "correctly answers to version commands" $
      executeCommand ss VersionCmd `shouldReturn` pack (showVersion P.version)

