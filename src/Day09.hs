module Day09 (
  day09
) where

import Common
import Data.Char (digitToInt)
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Data.Maybe (fromMaybe)
import qualified Data.HashSet as HS
import Data.List (sortBy)

type Input = V.Vector (V.Vector Int)
type Point = (Int, Int)

day09 :: AOCSolution
day09 input = show <$> [p1, part2 i]
  where
    i = parseInput input
    p1 = sum $ map (part1 i) $ getPoints i

getPoints :: Input -> [Point]
getPoints i = [(r, c) | r <- [0..pred rl], c <- [0..pred cl]]
  where
    rl = length i
    cl = length (i ! 0)

parseInput :: String -> Input
parseInput input = V.fromList $ map V.fromList $ map2 digitToInt $ lines input

lookupInInput :: Input -> Point -> Int
lookupInInput i (r, c) = fromMaybe 9 $ i !? r >>= flip (!?) c

getNeighbours :: Point -> [Point]
getNeighbours (r, c) = [(pred r, c), (succ r, c), (r, pred c), (r, succ c)]

part1 :: Input -> Point -> Int
part1 i p = if all (> value) neighbours then succ value else 0
  where
    value = lookupInInput i p
    neighbours = lookupInInput i <$> getNeighbours p

part2 :: Input -> Int
part2 i = product $ take 3 $ sortBy (flip compare) $ p2 points []
  where
    points = HS.fromList $ filter ((< 9) . lookupInInput i) $ getPoints i

    p2 :: HS.HashSet Point -> [Int] -> [Int]
    p2 ps g = if HS.null ps then g else p2 ps' g'
      where
        cp  = findConnectedPoints i [head $ HS.toList ps] HS.empty
        g'  = HS.size cp : g
        ps' = HS.difference ps cp

findConnectedPoints :: Input -> [Point] -> HS.HashSet Point -> HS.HashSet Point
findConnectedPoints _ [] v = v
findConnectedPoints i (p:s) v = findConnectedPoints i s' v'
  where
    n = filter ((< 9) . lookupInInput i) $
      filter (not . flip HS.member v) $
      getNeighbours p
    s' = s ++ n
    v' = HS.insert p v
