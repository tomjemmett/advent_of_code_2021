module Day01 (
  day01  
) where

import Common
import Data.List (tails)

day01 :: String -> String
day01 input = unlines $ countIncrements <$> [1, 3] <*> pure (linesRead input)

-- check that each item in the list is less than the nth next item
-- for part 1, n = 1 (the next item)
-- for part 2, n = 3: sum (a, b, c) < sum (b, c, d) iff a < d
countIncrements :: Int -> [Int] -> String
countIncrements n x = show $ countTrue id $ zipWith (<) x (drop n x)