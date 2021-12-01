module Day01 (
  day01  
) where

import Data.List (tails)

day01 :: String -> String
day01 input = unlines $ map (show . count_increments 0) [i, rollsum3 i]
  where
    i = map read $ lines input

-- for part 1, recursively run this function over the input to work out whether
-- to incremement the counter (n) or not
count_increments :: Int -> [Int] -> Int
-- base case: we have only one item left in the list, so just return n
count_increments n [_] = n
-- recursive case: take the first two items from the list (a and b), and compare
-- them. if b is greater than a then add one to n'.
count_increments n (a:b:xs)
  | b > a = 1 + n'
  | otherwise = n'
  where
    n' = count_increments n (b:xs) -- recursive call, drop a and run on b and the rest

-- for part 2 we need to implement a function that will first give us the rolling
-- window sum of width 3 of the input, then we can call the part 1 function
rollsum3 :: [Int] -> [Int]
rollsum3 = map sum
  . filter((3 ==) . length)
  . map (take 3)
  . tails