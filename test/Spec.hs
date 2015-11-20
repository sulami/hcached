import           Test.Hspec

import           LimitedHashMapSpec (lhmSpec)
import           ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
  lhmSpec
  parserSpec

