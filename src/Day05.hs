module Day05 (
  day05
) where

import Common
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ((!))
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Line  = (Point, Point)
type Lines = [Line]

day05 :: AOCSolution
day05 input = [p1 i, ""]
  where
    i = parseInput input

makeGrid :: M.Map (Int, Int) Int
makeGrid = M.fromList $ zip keys $ repeat 0
  where
    sz = [0..999]
    lg = length sz
    keys = concat $ map (zip sz . replicate lg) sz

parseInput :: String -> Lines
parseInput = map parseLine . filter (/= "") . lines
  where
    parseLine l = ((a, b), (c, d))
      where
        [[a, b], [c, d]] = map2 read . map (splitOn ",") $ splitOn " -> " l

p1 lines = show $ length $ filter((>= 2)) $ M.elems grid
  where
    lines' = filter (\((a, b), (c, d)) -> a == c || b == d) lines
    grid = foldl processLine makeGrid lines'

processLine grid line = grid'
  where
    grid' = foldl processPoint grid points
    points = lineToPoints line
    processPoint = flip (M.adjust (+1))

lineToPoints (ab@(a, b), cd@(c, d))
  | a == c && b <  d = ab : lineToPoints ((a, b + 1), cd)
  | a == c && b >  d = ab : lineToPoints ((a, b - 1), cd)
  | b == d && a <  c = ab : lineToPoints ((a + 1, b), cd)
  | b == d && a >  c = ab : lineToPoints ((a - 1, b), cd)
  | otherwise        = [ab]