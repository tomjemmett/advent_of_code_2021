module Day06 (
  day06
) where

import Common
import Data.List (group, sort)

-- we can solve today's problem by representing the input as a list of integers, representing the number of fish that
-- are alive on that day.
-- we take the input and seperate into a list of integers, then append the numbers 0 through 8 (this ensures each day
-- is listed, but does mean that we have 1 extra fish per day). if we sort the list, we can then group the numbers up
-- like: [0,1,1,2,2,2] => [[0], [1,1], [2,2,2]]
-- then, we can work out the length of each item, subtracting 1 from the value. The above example would become [0,1,2].
day06 :: AOCSolution
day06 input = show . sum . (!!) (iterate d6 i) <$> [80, 256]
  where
    i = map (pred . length) $ group $ sort $ commaSeparatedInts input ++ [0..8]

-- drop the first item from the input, shifting the days along to the left
-- then, add to the 6th day the value from day 0, and append day 0 to the end of the list
d6 :: [Int] -> [Int]
d6 (x:xs) = zipWith (+) [0,0,0,0,0,0,x,0] xs ++ [x]