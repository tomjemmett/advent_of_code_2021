module Day06 (
  day06
) where

import Common
import Data.List (group, sort)
import qualified Data.Map as M

day06 :: AOCSolution
day06 input = show . d6 x <$> [80, 256]
  where
    i = group $ sort $ commaSeparatedInts input
    m = foldr (`M.insert` 0 ) M.empty [0..8]
    x = M.elems $ foldr (\k -> M.insertWith (+) (head k) (length k)) m i

d6 :: [Int] -> Int -> Int
d6 xs 0 = sum xs
d6 (x:xs) d = d6 xs' $ pred d
  where
    xs' = zipWith (+) [0,0,0,0,0,0,x,0] xs ++ [x]