module HeapSpec (spec) where

import Test.Hspec
import Heap


test_insert_and_getMax:: Spec
test_insert_and_getMax =
  describe "insert and getMax" $ do
    it "returns the correct max element" $ do
      let h = singleton 1
      getMax h `shouldBe` 1

spec :: Spec
spec = do
   test_insert_and_getMax

-- For running the test stand-alone
main :: IO ()
main = hspec spec