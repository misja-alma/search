module HeapCheck (test) where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified PairingHeap as H

instance (Ord a, Arbitrary a) => Arbitrary (H.Heap a) where
  arbitrary = do
    ts <- arbitrary
    return (H.fromList ts)

prop_fromList_toList :: [Int] -> Bool
prop_fromList_toList xs = Set.fromList xs == Set.fromList ((H.toList . H.fromList) xs)

prop_insert_increases_heap :: H.Heap Int -> Int -> Bool
prop_insert_increases_heap h x = H.size h + 1 == H.size (H.insert x h)

prop_getMax_is_max :: H.Heap Int -> Bool
prop_getMax_is_max h = H.isEmpty h || H.getMax h == (maximum $ H.toList h)

prop_deleteMax_deletes_max :: H.Heap Int -> Bool
prop_deleteMax_deletes_max h = H.isEmpty h ||
   let currentMax = maximum $ H.toList h
   in (count currentMax h) - 1 == count currentMax (snd $ H.deleteMax h)
   where count m h' = length $ filter (== m) (H.toList h')

test = do
   quickCheck prop_insert_increases_heap
   quickCheck prop_fromList_toList
   quickCheck prop_getMax_is_max
   quickCheck prop_deleteMax_deletes_max