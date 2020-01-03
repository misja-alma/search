module SearchSpec (spec) where

import Test.Hspec
import Search


-- TODO test aStarSearch with a complex type that has a length or so, so we could compare it with bfs.
--      or somehow keep track of how many times the matchFunction is called and compare that. But then we need a monadic match function ..

test_aStarSearch :: Spec
test_aStarSearch =
  describe "aStarSearch" $ do
    it "finds the the first candidate that satisfies the matchFunction" $
      -- Note that the heap in a*search is min-based so we need an ordering that prefers the smallest candidate
      aStarSearch (1 :: Int) (\x -> [x+1, x-1]) (<=(-7)) `shouldBe` Just (-7)
    it "returns Nothing if no path found" $
      aStarSearch (0 :: Int) (\x -> if x < 10 then [x+1] else []) (==11) `shouldBe` Nothing

test_bfs :: Spec
test_bfs =
  describe "bfs" $ do
    it "finds the the first candidate that satisfies the matchFunction" $
      bfs (1 :: Int) (\x -> [x+1, x-1]) (>=7) `shouldBe` Just 7
    it "returns Nothing if no path found" $
      bfs (0 :: Int) (\x -> if x < 10 then [x+1] else []) (==11) `shouldBe` Nothing

test_BfsWithPath :: Spec
test_BfsWithPath =
  describe "bfsWithPath" $ do
    it "finds the shortest (reversed) path to a node satisfying the matchFunction" $ do
      bfsWithPath (1 :: Int) (\x -> [x+1, x-1]) (==1) `shouldBe` Just [1]
      bfsWithPath (1 :: Int) (\x -> [x+1, x-1]) (>=3) `shouldBe` Just [3,2,1]
    it "returns Nothing if no path found" $
      bfsWithPath (0 :: Int) (\x -> if x < 10 then [x+1] else []) (==11) `shouldBe` Nothing

spec :: Spec
spec = do
   test_aStarSearch
   test_bfs
   test_BfsWithPath

-- For running the test stand-alone
main :: IO ()
main = hspec spec
