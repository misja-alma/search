module SearchTest (test) where

import Search

-- bfs takes about twice as long as a* here. Makes sense because a* will only consider the negative branch
-- while bfs will each time consider both.
-- TODO now check usage hashset + performance; bfs takes 23 sec, a* 11 sec. (with power cable unplugged)
--      With HashSet: a*: 4.3 / 4.6 sec, bfs 8.4 / 9.7 sec!!
-- splay heap: 4.7 sec
test :: IO ()
test = do
   let result = aStarSearch (1 :: Int) (\x -> [x+1, x-1]) (>=(7000000))
   case result of
     Just x -> putStrLn $ "Solution found: " ++ show x
     Nothing -> putStrLn "No solution found"