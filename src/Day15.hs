module Day15 (
  day15
) where

import Common
import Control.Monad (liftM2)
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as S

type Graph = M.Map Point2d Int

day15 :: AOCSolution
day15 input = [show p1, ""]
  where
    i = parseInput input
    size = length (lines input) - 1
    p1 = bfs i (size, size) S.empty (PQ.singleton 0 (0, 0))

parseInput :: String -> Graph
parseInput input = M.fromList (zip p (concat i))
  where
    i = map2 digitToInt $ lines input
    p = concat $ (zipWith3 . zipWith3) (const (,)) i (repeat [0..]) (map repeat [0..])

bfs :: Graph -> Point2d -> S.HashSet Point2d -> PQ.MinPQueue Int Point2d -> Int
bfs g t v pq
  | s == t    = d + (g ! t) - (g ! (0, 0))
  | otherwise = bfs g t v' pq''
  where
    ((d, s), pq') = PQ.deleteFindMin pq
    d' = (g ! s) + d
    n = filter ((not . flip S.member v) <&&> flip M.member g) $ point2dNeighbours s
    pq'' = foldr (uncurry PQ.insert . (d',)) pq' n
    v' = S.insert s v

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftM2 (&&)
