module Day07 (
  day07
) where

import Common
import Data.List (sort)

day07 :: AOCSolution
day07 input = show <$> [p1, p2]
  where
    i  = sort $ commaSeparatedInts input
    -- find the median value, this will solve for part 1
    p1 = d7 i id $ i !! (length i `div` 2)
    -- for part 2 we check with the values either side of the mean
    p2 = minimum $ d7 i triangleN <$> ([id, succ] <*> pure (sum i `div` length i))

-- day 7 solver
d7 :: [Int] -> (Int -> Int) -> Int -> Int
d7 i f a = sum $ map (f . abs . (-) a) i

-- calculate triangle number
triangleN :: Int -> Int
triangleN n = n * (n + 1) `div` 2
