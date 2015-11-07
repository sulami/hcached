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

