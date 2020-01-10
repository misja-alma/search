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
--      the heap becomes a linear tree. This is because either smaller or bigger will return an empty heap every time:
--      meaning a heap will be formed where one side is always empty.
--      this solution makes things a bit better. However it doesn't really show a speedup in practice, I guess because most data is pretty random anyway.
rebalance :: Ord a => Heap a -> Heap a
rebalance (Heap (Heap left x' Empty) x Empty)  = Heap left x' (singleton x)
rebalance (Heap Empty x (Heap Empty x' right)) = Heap (singleton x) x' right
rebalance h = h

insert :: Ord a => a -> Heap a -> Heap a
insert x h = let (smaller, bigger) = partition x h
             in rebalance $ Heap smaller x bigger

partition :: Ord a => a -> Heap a -> (Heap a, Heap a)
partition pivot Empty = (Empty, Empty)
partition pivot t@(Heap left x right) =
   if x > pivot then
      case left of
         Empty  -> (Empty, rebalance t)
         t2     -> let (smaller, bigger) = partition pivot t2
                   in (rebalance smaller, rebalance $ Heap bigger x right)
   else
     case right of
        Empty -> (rebalance t, Empty)
        t2    -> let (smaller, bigger) = partition pivot t2  
                 in (rebalance $ Heap left x smaller, rebalance bigger)

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
deleteMax (Heap left x (Heap left' x' Empty)) = (x', Heap left x left')  -- rebalance a bit
deleteMax (Heap left x (Heap left' x' right')) =
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
