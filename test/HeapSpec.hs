module HeapSpec (spec) where

import Test.Hspec
import SplayHeap
import qualified Data.Set as Set

test_insert_and_getMax:: Spec
test_insert_and_getMax =
  describe "getMax" $ do
    it "returns the only element from a singleton heap" $ do
      let h = singleton 1
      getMax h `shouldBe` 1
    it "returns the biggest element from a larger heap" $ do
      let h = insert 2 (singleton 1)
      getMax h `shouldBe` 2
      let h2 = insert 3 h
      getMax h2 `shouldBe` 3
      let h3 = insert (-1) h2
      getMax h3 `shouldBe` 3

test_deleteMax =
  describe "deleteMax" $ do
    it "deletes the only element of a singleton heap" $ do
      let h = singleton 1
      let (m, h2) = deleteMax h
      m `shouldBe` 1
      isEmpty h2 `shouldBe` True
    it "deletes the biggest element of a  heap" $ do
      let h = fromList [1,-1,3,5,2,2]
      let (m, h2) = deleteMax h
      m `shouldBe` 5
      getMax h2 `shouldBe` 3
      let (m2, h3) = deleteMax h2
      m2 `shouldBe` 3
      getMax h3 `shouldBe` 2
      let (m3, h4) = deleteMax h3
      m3 `shouldBe` 2
      getMax h4 `shouldBe` 2

test_from_and_to_list:: Spec
test_from_and_to_list =
  describe "fromList and toList" $ do
    it "give back the same elements after applying both" $ do
      let h = fromList [1,2,3]
      let l = toList h
      Set.fromList l `shouldBe` Set.fromList [1,2,3]


spec :: Spec
spec = do
   test_insert_and_getMax
   test_deleteMax
   test_from_and_to_list

-- For running the test stand-alone
main :: IO ()
main = hspec spec