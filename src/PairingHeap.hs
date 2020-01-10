-- Invariant: in Heap x xs, x >= all xs, also none of xs is Empty.

module PairingHeap (insert,
             deleteMax,
             getMax,
             fromList,
             toList,
             isEmpty,
             empty,
             singleton,
             size,
             Heap (..) ) where


data Heap a = Empty | Heap a [Heap a]

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

empty :: Heap a
empty = Empty

singleton :: a -> Heap a
singleton x = Heap x []

size :: Heap a -> Int
size Empty = 0
size (Heap a as) = 1 + sum (fmap size as)

getMax :: Heap a -> a
getMax (Heap x _) = x

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Heap x xs) h2@(Heap y ys) = if x >= y then Heap x (h2 : xs) else Heap y (h1 : ys)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = Empty
mergePairs [x] = x
mergePairs (x1 : x2 : xs) = merge (merge x1 x2) (mergePairs xs)

deleteMax :: Ord a => Heap a -> (a, Heap a)
deleteMax (Heap x xs) = (x, mergePairs xs)

fromList :: Ord a => [a] -> Heap a
fromList = foldl (\h x -> insert x h) empty

toList :: Heap a -> [a]
toList Empty = []
toList (Heap x xs) = x : (xs >>= toList)

instance (Show a) => Show (Heap a) where
    show tr = ((++) "\n" . unlines . toLines) tr

        where

        toLines :: (Show a) => Heap a -> [String]
        toLines Empty            = [""]
        toLines (Heap t [])      = [show t]
        toLines (Heap t xs)      = [show t] ++ ["****"] ++ (("    " ++) <$> (xs >>= toLines))