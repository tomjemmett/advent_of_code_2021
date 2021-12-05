module Day05 (
  day05
) where

import Common
import qualified Data.Map as M
import Data.Map ((!))
import Data.List.Split (splitOn)

type Point  = (Int, Int)
type Points = [Point]
type Line   = (Point, Point)
type Lines  = [Line]
type Grid   = M.Map Point Int

day05 :: AOCSolution
day05 input = show <$> d5 <$> ([p1, p2] <*> pure (parseInput input))
  where
    p1 = filter (\((a, b), (c, d)) -> a == c || b == d)
    p2 = id

parseInput :: String -> Lines
parseInput = map parseLine . filter (/= "") . lines
  where
    parseLine l = ((a, b), (c, d))
      where
        [[a, b], [c, d]] = map2 read . map (splitOn ",") $ splitOn " -> " l

d5 :: Lines -> Int
d5 = length .
  filter((>= 2)) .
  M.elems .
  foldl processLine M.empty

processLine :: Grid -> Line -> Grid
processLine grid line = grid'
  where
    grid' = foldl processPoint grid points
    points = lineToPoints line
    processPoint = flip $ flip (M.insertWith (+)) 1

lineToPoints :: Line -> Points
lineToPoints (ab@(a, b), cd@(c, d))
  | a == c && b == d = [ab]
  | a == c           = ab : lineToPoints ((a, f b d), cd)
  | b == d           = ab : lineToPoints ((f a c, b), cd)
  | otherwise        = ab : lineToPoints ((f a c, f b d), cd)
  where f x y = (if x < y then succ else pred) x