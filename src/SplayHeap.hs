-- The invariant of a SplayHeap is that for every subheap the left node contains only smaller elements and the right node only bigger.

module SplayHeap (insert,
             deleteMax,
             getMax,
             fromList,
             toList,
             isEmpty,
             empty,
             singleton,
             size,
             Heap (..) ) where


data Heap a = Empty | Heap (Heap a) a (Heap a)

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

empty :: Heap a
empty = Empty

size :: Heap a -> Int
size Empty = 0
size (Heap left _ right) = 1 + size left + size right

singleton :: a -> Heap a
singleton x = Heap Empty x Empty

-- TODO problem with Okasaki's implementation is that when repeatedly inserting a new biggest/ smallest element,
--      the tree becomes essentially linear. This is because either smaller or bigger will return an empty heap every time:
--      meaning a heap will be formed where one side is always empty.
--      this solution makes things a bit better but still not 100%
rebalance :: Ord a => Heap a -> Heap a
rebalance (Heap (Heap left x' Empty) x Empty)  = Heap left x' (singleton x)
rebalance (Heap Empty x (Heap Empty x' right)) = Heap (singleton x) x' right
rebalance h = h

insert :: Ord a => a -> Heap a -> Heap a
insert x h = rebalance $ Heap (smaller x h) x (bigger x h)

-- TODO combine smaller and bigger
smaller :: Ord a =>  a -> Heap a -> Heap a
smaller pivot Empty = Empty
smaller pivot (Heap left x right) =
   if x > pivot then smaller pivot left
   else case right of
           Empty                -> Heap left x Empty
           Heap left' x' right' ->
              if x' > pivot then rebalance $ Heap left x (smaller pivot left')
              else rebalance $ Heap (Heap left x left') x' (smaller pivot right')

bigger :: Ord a => a -> Heap a -> Heap a
bigger pivot Empty = Empty
bigger pivot (Heap left x right) =
   if x <= pivot then bigger pivot right
   else case left of
           Empty                -> Heap Empty x right
           Heap left' x' right' ->
              if x' <= pivot then rebalance $ Heap (bigger pivot right') x right
              else rebalance $ Heap (bigger pivot left') x' (Heap right' x right)

fromList :: Ord a => [a] -> Heap a
fromList = foldl (\h x -> insert x h) empty

toList :: Heap a -> [a]
toList Empty = []
toList (Heap left x right) = (toList left) ++ [x] ++ (toList right)

getMax :: Heap a -> a
getMax Empty = error "empty heap"
getMax (Heap _ x Empty) = x
getMax (Heap _ _ bs) = getMax bs

deleteMax :: Heap a -> (a, Heap a)
deleteMax Empty = error "empty heap"
deleteMax (Heap left x Empty) = (x, left)
deleteMax (Heap left x (Heap left' x' Empty)) = (x', Heap left x left')
deleteMax (Heap left x (Heap left' x' right')) =  -- rebalance a bit
   let (m, right'') = deleteMax right'
   in (m, Heap left x (Heap left' x' right''))


instance (Show a) => Show (Heap a) where
    show tr = ((++) "\n" . unlines . snd . toLines) tr

        where

        toLines :: (Show a) => Heap a -> (Int, [String])
        toLines Empty                 = (0, [""])
        toLines (Heap Empty t Empty)  = (0, [" " ++ show t])
        toLines (Heap l t r)          = (ln + 1, lv_new ++ [" *"] ++ [" " ++ show t] ++ [" *"] ++ rv_new)
            where
            (il, lv)    = toLines l
            (ir, rv)    = toLines r
            ln          = length lv
            rn          = length rv
            lv_sub      = (replicate il "        ") ++ [" *******"] ++ (replicate (ln - il) " *      ")
            rv_sub      = (replicate ir " *      ") ++ [" *******"] ++ (replicate (rn - ir) "        ")
            lv_new      = zipWith (++) lv_sub lv
            rv_new      = zipWith (++) rv_sub rv
