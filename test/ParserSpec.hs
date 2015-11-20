{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Data.Either (isLeft, isRight)

import           Test.Hspec

import           Command (parse)

parserSpec :: Spec
parserSpec = describe "Command Parser" $ do
  it "parses basic valid commands" $ do
    parse "set 1 key value\n" `shouldSatisfy` isRight
    parse "get key\n" `shouldSatisfy` isRight
    parse "delete key\n" `shouldSatisfy` isRight

  it "parses set requests with multi-word values" $
    parse "set 1 key value more values\n" `shouldSatisfy` isRight

  it "does not parse empty requests" $
    parse "\n" `shouldSatisfy` isLeft

  it "does not parse requests without terminating newlines" $
    parse "get something" `shouldSatisfy` isLeft

  it "does not parse single-argument-commands with extraneous arguments" $ do
    parse "get key extra\n" `shouldSatisfy` isLeft
    parse "delete key extra\n" `shouldSatisfy` isLeft

  it "does not parse set commands with non-numerical TTLs" $
    parse "set 0xABC key value\n" `shouldSatisfy` isLeft

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

