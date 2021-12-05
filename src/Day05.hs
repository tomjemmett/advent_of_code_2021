module Day05 (
  day05
) where

import Common
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ((!))
import Data.List.Split (splitOn)

type Point  = (Int, Int)
type Points = [Point]
type Line   = (Point, Point)
type Lines  = [Line]
type Grid   = M.Map Point Int

day05 :: AOCSolution
day05 input = [p1 i, p2 i]
  where
    i = parseInput input

makeGrid :: Grid
makeGrid = M.empty

parseInput :: String -> Lines
parseInput = map parseLine . filter (/= "") . lines
  where
    parseLine l = ((a, b), (c, d))
      where
        [[a, b], [c, d]] = map2 read . map (splitOn ",") $ splitOn " -> " l

p1 :: Lines -> String
p1 = p2 . filter (\((a, b), (c, d)) -> a == c || b == d)

p2 :: Lines -> String
p2 lines = show $ length $ filter((>= 2)) $ M.elems grid
  where
    grid = foldl processLine makeGrid lines

processLine :: Grid -> Line -> Grid
processLine grid line = grid'
  where
    grid' = foldl processPoint grid points
    points = lineToPoints line
    processPoint = flip $ flip (M.insertWith (+)) 1

lineToPoints :: Line -> Points
lineToPoints (ab@(a, b), cd@(c, d))
  | a == c && b <  d = ab : lineToPoints ((a, b + 1), cd)
  | a == c && b >  d = ab : lineToPoints ((a, b - 1), cd)
  | b == d && a <  c = ab : lineToPoints ((a + 1, b), cd)
  | b == d && a >  c = ab : lineToPoints ((a - 1, b), cd)
  | a <  c && b <  d = ab : lineToPoints ((a + 1, b + 1), cd)
  | a <  c && b >  d = ab : lineToPoints ((a + 1, b - 1), cd)
  | a >  c && b <  d = ab : lineToPoints ((a - 1, b + 1), cd)
  | a >  c && b >  d = ab : lineToPoints ((a - 1, b - 1), cd)
  | otherwise        = [ab]