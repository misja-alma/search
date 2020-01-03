module Search (bfs, bfsWithPath, aStarSearch) where

import Data.Maybe
import qualified Data.Sequence as D
import qualified Data.Set as S
import qualified Data.HashSet as HS   -- (NOTE: from hashmap, not from unordered-containers)
import qualified Data.Heap.Splay as H -- (NOTE: from heaps, not from heap!).
import Data.Hashable

dequeue :: D.Seq a -> (a, D.Seq a)
dequeue sq = let (rest, el) = D.splitAt (D.length sq - 1) sq in (D.index el 0, rest)

-- TODO move to Search module
-- TODO benchmark with other heap implementations. E.g. splay heap, pairing heap, fibonacci heap.

uncons :: H.Heap a -> Maybe (a, H.Heap a)
uncons h = fmap (\x -> (x, H.deleteMin h)) (H.minimum h)

-- NOTE: uses a heap that is min-based! So the smallest candidate according to the ordering is always selected first.
aStarSearch :: Hashable a => Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe a
aStarSearch root getChildren matchFunction =
    let queue = H.singleton root
        visited = HS.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (H.null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let Just (candidate, poppedQ) = uncons q in
                                    if matchFunction candidate then (poppedQ, v, Just candidate)
                                    else let children = filter (`HS.notMember` v) (getChildren candidate)
                                             newVisited = HS.union v (HS.fromList children)
                                             nextQueue = foldl (\newQ c -> H.insert c newQ) poppedQ children in
                                         (nextQueue, newVisited, Nothing)

bfs :: Hashable a => Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe a
bfs root getChildren matchFunction =
    let queue = D.singleton root
        visited = HS.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (D.null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let (candidate, poppedQ) = dequeue q in
                                    if matchFunction candidate then (poppedQ, v, Just candidate)
                                    else let children = filter (`HS.notMember` v) (getChildren candidate)
                                             newVisited = HS.union v (HS.fromList children)
                                             nextQueue = foldl (\newQ c -> (D.<|) c newQ) poppedQ children in
                                         (nextQueue, newVisited, Nothing)

-- while queue not empty: take bottom el, eval with utilityFunction, if finished exit
-- otherwise get children, filter on not visited, add then to visited and to front of queue
-- Returns the reversed path with the matched element at the top. Or nothing if not found.
-- NOTE this could also be accomplished by the simple bfs function above: in that case getChildren should return paths and the match function should check the heads of the paths.
bfsWithPath :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
bfsWithPath root getChildren matchFunction =
    let queue = D.singleton [root]
        visited = S.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (D.null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let (path, poppedQ) = dequeue q
                                        candidate = head path in
                                    if matchFunction candidate then (poppedQ, v, Just path)
                                    else let children = filter (`S.notMember` v) (getChildren candidate)
                                             newVisited = S.union v (S.fromList children)
                                             nextQueue = foldl (\newQ c -> (D.<|) (c : path) newQ) poppedQ children in
                                         (nextQueue, newVisited, Nothing)
