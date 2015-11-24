{-# LANGUAGE OverloadedStrings #-}

module ServerSpec where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (newMVar, readMVar)
import           Data.Maybe (isJust)
import           System.IO (BufferMode (..), hGetLine, hPutStr, hSetBuffering)

import           Control.Lens (view)
import           Network (PortID (..), connectTo)
import           Test.Hspec

import           LimitedHashMap (get', set)
import           Server (initialState, janitor, lhm, runServer)

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

