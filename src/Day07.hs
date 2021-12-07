module Day07 (
  day07
) where

import Common
import Data.List (sort)

day07 :: AOCSolution
day07 input = (show . minimum . (d7' <*>)) . pure <$> [id, triangleN]
  where
    i  = sort $ commaSeparatedInts input
    -- find the median value, this will solve for part 1
    median  = i !! (length i `div` 2)
    -- use the d7 solver, start with our initial estimate for the solution as the largest possible INt (maxBound)
    -- and start trying to solve from the median value
    -- we create two copies of this function, one that searches left (pred) and one that searches right (succ)
    d7' = d7 maxBound median i <$> [pred, succ]

-- d7 solver: takes a current estimate of the solution, the current position of the crabs, the input, then two functions
-- updatePos should either be pred or succ and will move pos one step to the left or right
-- cost is the cost function, for part 1 this is the identity function, but for part 2 we use the triangle number
-- function.
-- each time we run the function we calculate the new candidate solution, s', for this position. if this is a smaller
-- value than the current possible solution, then we iterate and run the function again.
-- eventually, we will reach a point where the next candidate solution will be greater than the previous one, at that
-- point we have found the best solution (in that direction)
d7 :: Int -> Int -> [Int] -> (Int -> Int) -> (Int -> Int) -> Int
d7 s pos i updatePos cost
  | s' < s    = d7 s' (updatePos pos) i updatePos cost
  | otherwise = s
  where
    s' = sum $ map (cost . abs . (-) pos) i

-- calculate triangle number
triangleN :: Int -> Int
triangleN n = n * (n + 1) `div` 2
