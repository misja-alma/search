module SplayHeap (insert,
             deleteMax,
             getMax,
             fromList,
             toList,
             isEmpty,
             empty,
             singleton,
             Heap (..) ) where


data Heap a = Empty | Heap (Heap a) a (Heap a)

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

empty :: Heap a
empty = Empty

singleton :: a -> Heap a
singleton x = Heap Empty x Empty

insert :: Ord a => a -> Heap a -> Heap a
insert x h = Heap (smaller x h) x (bigger x h)

smaller :: Ord a =>  a -> Heap a -> Heap a
smaller pivot Empty = Empty
smaller pivot (Heap left x right) =
   if x > pivot then smaller pivot left
   else case right of
           Empty                -> Heap left x Empty
           Heap left' x' right' ->
              if x' > pivot then Heap left x (smaller pivot left')
              else Heap (Heap left x left') x' (smaller pivot right')

bigger :: Ord a => a -> Heap a -> Heap a
bigger pivot Empty = Empty
bigger pivot (Heap left x right) =
   if x <= pivot then bigger pivot right
   else case left of
           Empty                -> Heap Empty x right
           Heap left' x' right' ->
              if x' <= pivot then Heap (bigger pivot left') x right
              else Heap (bigger pivot left') x' (Heap right' x right)

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
deleteMax (Heap left x right) =
   let (m, right') = deleteMax right
   in (m, Heap left x right')


