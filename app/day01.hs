module Day01 (
  day01  
) where

import Data.List (tails)

day01 :: String -> String
day01 i = unlines [p1, p2]
  where
    p1 = show $ day01_p1 0 input
    p2 = show $ day01_p1 0 $ day01_p2 input
    input = map read $ lines i

-- for part 1, recursively run this function over the input to work out whether
-- to incremement the counter (n) or not
day01_p1 :: Int -> [Int] -> Int
-- base case: we have only one item left in the list, so just return n
day01_p1 n [_] = n
-- recursive case: take the first two items from the list (a and b), and compare
-- them. if b is greater than a then add one to n'.
day01_p1 n (a:b:xs)
  | b > a = 1 + n'
  | otherwise = n'
  where
    n' = day01_p1 n (b:xs) -- recursive call, drop a and run on b and the rest

-- for part 2 we need to implement a function that will first give us the rolling
-- window sum of width 3 of the input, then we can call the part 1 function
day01_p2 :: [Int] -> [Int]
day01_p2 = map sum
  . filter((3 ==) . length)
  . map (take 3)
  . tails