module Day15 (
  day15
) where

import Common
import Control.Monad (liftM2)
import Data.Char (digitToInt)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as S
import qualified Data.Vector as V
import Data.Vector ((!))

type Graph = V.Vector (V.Vector Int)

day15 :: AOCSolution
day15 input = [show p1, ""]
  where
    i = parseInput input
    size = length (lines input) - 1
    p1 = bfs i (size, size) S.empty (PQ.singleton 0 (0, 0))

parseInput :: String -> Graph
parseInput = V.fromList . map V.fromList . map2 digitToInt . lines

inBounds :: Graph -> Point2d -> Bool
inBounds g (px, py) = px >= 0 && px <= mx && py >= 0 && py <= my
  where
    mx = pred $ length g
    my = pred $ length $ g ! 0

bfs :: Graph -> Point2d -> S.HashSet Point2d -> PQ.MinPQueue Int Point2d -> Int
bfs g t@(tx, ty) v pq
  | s == t    = d + (g ! tx ! ty) - (g ! 0 ! 0)
  | otherwise = bfs g t v' pq''
  where
    ((d, s@(sx, sy)), pq') = PQ.deleteFindMin pq
    d' = (g ! sx ! sy) + d
    n = filter ((not . flip S.member v) <&&> inBounds g) $ point2dNeighbours s
    pq'' = foldr (uncurry PQ.insert . (d',)) pq' n
    v' = S.insert s v

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftM2 (&&)
