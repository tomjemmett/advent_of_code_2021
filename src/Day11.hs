module Day11 (
  day11
) where

import Common
import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as M
import Data.List (sortBy, transpose, (\\))
import Data.Function (on)

type D10Map = M.Map Point2d Int

day11 :: AOCSolution
day11 input = map show $ [p1, p2] <*> pure (iterate step (parseInput input, 0))
  where
    p1 = sum . take 101 . map snd
    p2 = length . takeWhile ((< 100) . snd)

parseInput :: String -> D10Map
parseInput input = M.fromList $ zip p $ concat i
  where
    i = map2 digitToInt $ lines input
    p = getPoints i

getPoints :: [[Int]] -> [Point2d]
getPoints = concat . (zipWith3 . zipWith3) (curry const) (repeat [0..]) (map repeat [0..])

step :: (D10Map, Int) -> (D10Map, Int)
step (g, _) = (g', f')
  where
    -- initial increase of everything by 1
    (g', f') = substep (M.map succ g) []
    -- then perform the iterative step
    substep :: D10Map -> [Point2d] -> (D10Map, Int)
    substep g v = if null k then (g0, f) else substep g' (v ++ k)
      where
        k  = M.keys (M.filter (> 9) g) \\ v
        n  = concatMap point2dNeighboursDiags k
        g' = foldl (flip $ M.adjust succ) g n
        g0 = M.map (\x -> if x > 9 then 0 else x) g
        f  = M.size $ M.filter (== 0) g0