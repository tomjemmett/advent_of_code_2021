module Day08 (
  day08, parseInput
) where

import Common
import Data.List (sort, intersect, union, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map ((!))

day08 :: AOCSolution
day08 input = [show $ foldl part1 0 i, show $ sum $ map part2 i]
  where
    i = parseInput input

part1 :: Int -> [[String]] -> Int
part1 n [_, b] = n + b'
  where
    b' = countTrue (`elem` [2, 4, 3, 7]) $ map length b

part2 [a, b] = sum $ zipWith (*) [1000, 100, 10, 1] $ map (m !) $ map sort b
  where
    fl n = filter (\x -> length x == n)
    a1 = head $ fl 2 a
    a4 = head $ fl 4 a
    a7 = head $ fl 3 a
    a8 = head $ fl 7 a
    l5 = fl 5 a
    l6 = fl 6 a
    -- first, consider what's same between a1 and a7, that should tell us what is 'a'
    pa = head $ a7 \\ a1
    -- c/f can be found from the intersection of a1 and a7
    pcf = a1 `intersect` a7
    -- b/d can be found from difference between a4 and a1, but can't tell their positions
    pbd = a4 \\ a1
    -- e/g can be found from difference between a8 and a7 and a4
    peg = a8 \\ (a4 `union` a7)
    -- we can find 3 now: it's going to be the only 5 length with f set
    a3 = head $ filter (\x -> length (x \\ pcf) == 3) l5
    -- which leads to g and d
    pg = head $ a3 \\ (a4 `union` a7)
    pd = head $ a3 \\ (a7 `union` [pg])
    -- we can now solve d and find 2 and 5
    pe = head $ head $ fl 1 $ map (\\ (a4 `union` [pa, pd, pg])) (l5 \\ [a3])
    a2 = head $ filter (pe `elem`) l5
    a5 = head $ l5 \\ [a2, a3]
    -- with 5 we can find b and c
    pb = head $ a5 \\ (a1 `union` [pa, pd, pg])
    pc = head $ a1 \\ a5
    -- f is now findable
    pf = head $ a8 \\ [pa, pb, pc, pd, pe, pg]
    -- now we can reconstruct the rest
    a0 = head $ filter (pd `notElem`) l6
    a6 = head $ filter (pe `elem`) (l6 \\ [a0])
    a9 = head $ l6 \\ [a0, a6]

    m = M.fromList [
      (sort a0, 0),
      (sort a1, 1),
      (sort a2, 2),
      (sort a3, 3),
      (sort a4, 4),
      (sort a5, 5),
      (sort a6, 6),
      (sort a7, 7),
      (sort a8, 8),
      (sort a9, 9)]



parseInput :: String -> [[[String]]]
parseInput = map (map words . splitOn "|") . lines

segmentN s = case sort s of
  "abcefg"  -> 0
  "cf"      -> 1
  "acdeg"   -> 2
  "acdfg"   -> 3
  "bcdf"    -> 4
  "abdfg"   -> 5
  "abdefg"  -> 6
  "acf"     -> 7
  "abcdefg" -> 8
  "abcdfg"  -> 9

t = \case
  'a' -> 'a'
  'c' -> 'b'
  'e' -> 'c'
  'd' -> 'd'
  'g' -> 'e'
  'f' -> 'f'
  'b' -> 'g'