module Day08 where
{-  
(
  day08
) where
-}

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
    -- 'c' and 'f' can be found from the intersection of a1 and a7
    pcf = a1 `intersect` a7
    -- we can find 3 now: it's going to be the only 5 length with an 'f'
    a3 = hfilter (\x -> length (x \\ pcf) == 3) l5
    -- which leads to 'g' and 'd'
    pg = a3 \\ (a4 `union` a7)
    pd = a3 \\ (a7 `union` pg)
    -- we can now solve 'e' and find 2 and 5
    pe = hfilter (not . null) $ map (\\ (foldr1 union [a4, pa, pd, pg])) (l5 \\ [a3])
    a2 = hfilter (not . null . intersect pe) l5
    a5 = head $ l5 \\ [a2, a3]
    -- now we can reconstruct the rest
    a0 = hfilter (null . intersect pd) l6
    a6 = hfilter (not . null . intersect pe) (l6 \\ [a0])
    a9 = head $ l6 \\ [a0, a6]
    -- create a map so we can easily lookup using b
    m = M.fromList $ zip [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] [0..]
