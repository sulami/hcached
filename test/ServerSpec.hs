module ServerSpec where

import           Control.Concurrent (forkIO)
import           System.IO (BufferMode (..), hGetLine, hPutStr, hSetBuffering)

import           Network (PortID (..), connectTo)
import           Test.Hspec

import           Server (initialState, runServer)

spec :: Spec
spec = do
  istate <- runIO $ initialState False 3
  runIO . forkIO $ runServer istate 11212

  it "answers to requests on its port" $ do
    handle <- connectTo "localhost" $ PortNumber 11212
    hSetBuffering handle LineBuffering
    hPutStr handle "get key\n"
    hGetLine handle `shouldReturn` "NOT_FOUND\r"

