module Day15 (
  day15
) where

import Common
import Data.Array.Unboxed (UArray, (!), bounds, inRange, listArray)
import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ

type Node = (Int, Point2d)
type Grid = UArray Point2d Int
type PathCosts = M.Map Point2d Int

day15 :: AOCSolution 
day15 input = show . d15 . toArray <$> ([id, expandGrid] <*> pure (parseInput input))

parseInput :: String -> [[Int]]
parseInput = map2 digitToInt . lines

expandGrid :: [[Int]] -> [[Int]]
expandGrid = concat .
  take 5 .
  iterate incrGraph .
  foldr1 (zipWith (++)) .
  take 5 .
  iterate incrGraph
  where
    incrGraph = map2 (succ . flip mod 9)

toArray :: [[Int]] -> Grid
toArray grid = listArray ((0, 0), (d, d)) (concat grid)
  where
    d = pred $ length grid

d15 :: Grid -> Int
d15 graph = dijkstra t neighbors (PQ.singleton 0 s) (M.singleton s 0)
  where
    (s, t) = bounds graph
    neighbors (r, p) = [(r + graph ! p', p') | p' <- point2dNeighbours p, inRange (s, t) p']

dijkstra :: Point2d -> (Node -> [Node]) -> PQ.MinPQueue Int Point2d -> PathCosts -> Int
dijkstra target neighbors pq pc
  | snd n == target = pc M.! target
  | otherwise = dijkstra target neighbors pq'' pc'
  where
    (n, pq') = PQ.deleteFindMin pq
    ns = filter (\(r, p) -> maybe True (r <) $ M.lookup p pc) $ neighbors n
    pc' = foldr (uncurry (flip M.insert)) pc ns
    pq'' = foldr (uncurry PQ.insert) pq' ns
