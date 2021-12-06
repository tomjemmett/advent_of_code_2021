module Day06 (
  day06
) where

import Common
import Data.List (group, sort)

day06 :: AOCSolution
day06 input = show . d6 i <$> [80, 256]
  where
    -- take the input, and append the full list of days (0 to 8)
    -- then, sort this list, group the items, and calculate the length
    -- this will tell us how many of each day appears in our input, but
    -- because we added all days we need to subtract one from the length
    --
    -- that is, we have a list counting how many fish are alive on each day
    i = map (pred . length) $ group $ sort $ commaSeparatedInts input ++ [0..8]

d6 :: [Int] -> Int -> Int
d6 xs 0 = sum xs -- reached last day, return the sum of our count list
d6 (x:xs) d = d6 xs' $ pred d
  where
    -- drop the first item from the input, shifting the days along to the left
    -- then, add to the 6th day the value from day 0, and append day 0 to the
    -- end of the list
    xs' = zipWith (+) [0,0,0,0,0,0,x,0] xs ++ [x]