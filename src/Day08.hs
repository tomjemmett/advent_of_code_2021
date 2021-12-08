module Day08 (
  day08
) where

import Common
import Data.List (sort, sortBy, groupBy, intersect, union, (\\))
import Data.List.Split (splitOn)
import Data.Function (on)
import qualified Data.Map as M
import Data.Map ((!))

day08 :: AOCSolution
day08 input = show <$> (foldl <$> [part1, part2] <*> pure 0 <*> pure i)
  where
    i = map (splitOn ["|"] . map sort . words) $ lines input

part1 :: Int -> [[String]] -> Int
part1 n [_, b] = n + (countTrue (`elem` [2, 4, 3, 7]) $ map length b) 

part2 :: Int -> [[String]] -> Int
part2 n [a, b] = n + foldl (\x y -> x * 10 + (m ! y)) 0 b
  where
    -- head filter: get the first thing filter finds
    hfilter f = head . (filter f)
    -- group the inputs on length
    [l2, l3, l4, l5, l6, l7] = groupBy ((==) `on` length) $ sortBy (compare `on` length) a
    -- these are the initial values that we know because they are unique
    [a1, a7, a4, a8] = head <$> [l2, l3, l4, l7]
    -- first, consider what's same between a1 and a7, that should tell us what is 'a'
    pa = a7 \\ a1
    -- 'g' can be found: it will be the only remaining character in 3 and 5 if we remove 4 and 'a'
    pg = hfilter ((== 1) . length) $ map (\\ pa `union` a4) l5
    -- from this, we can construct 9 and find 'e'
    a9 = sort $ foldl1 union [pa, pg, a4]
    pe = a8 \\ a9
    -- 'd' can be found: it will be the only remaining character in 3 if we remove 7 and 'g'
    pd = foldl1 intersect $ map (\\ a7 `union` pg) l5
    -- this allows us to reconstruct 3
    a3 = sort $ foldl1 union [pd, pg, a7]
    -- 2 is the only one of 2/3/5 with an 'e'
    a2 = hfilter (not . null . intersect pe) l5
    -- so we can now find 5
    a5 = head $ l5 \\ [a2, a3]
    -- now we can reconstruct the rest
    a0 = a8 \\ pd
    a6 = head $ (l6 \\ [a0]) \\ [a9]
    -- create a map so we can easily lookup using b
    m = M.fromList $ zip [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] [0..]
