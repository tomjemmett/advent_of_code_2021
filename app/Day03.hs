module Day03 (
  day03
) where

import Common
import Data.List (sort, group)
import Data.Char (digitToInt)

testInput = ["00100",
             "11110",
             "10110",
             "10111",
             "10101",
             "01111",
             "00111",
             "11100",
             "10000",
             "11001",
             "00010",
             "01010"]

day03 :: String -> String
day03 input = unlines [p1, p2]
  where
    i = lines input
    p1 = part1 i
    p2 = part2 i

part1 :: [String] -> String
part1 input = show $ gamma * epsillon
  where
    p1 :: [String] -> [Int]
    p1 i = foldl fn (repeat 0) i

    a = p1 input

    gamma = bitsToInt $ map (\x -> if x > 0 then 1 else 0) a
    epsillon = bitsToInt $ map (\x -> if x < 0 then 1 else 0) a

    fn :: [Int] -> String -> [Int]
    fn _ [] = []
    fn (y:ys) (x:xs) = y' : fn ys xs
      where y' = y + if x == '0' then -1 else 1

part2 :: [String] -> String 
part2 input = show $ oxygen input * co2Scruber input
  where
    getInt :: String -> Int
    getInt = bitsToInt . map digitToInt

    oxygen :: [String] -> Int
    oxygen = getInt . p2 (>) ""
    co2Scruber :: [String] -> Int
    co2Scruber = getInt . p2 (<=) ""

    p2 :: (Int -> Int -> Bool) -> String -> [String] -> String
    p2 comparer c [i] = c ++ i -- single element, return it
    p2 comparer c input = p2 comparer c' nextInput -- recursively call
      where
        -- consider just the first character
        firstChars = map head input
        -- now find the number of 0's and 1's
        zeros = length $ filter (== '0') firstChars
        ones  = length $ filter (== '1') firstChars
        -- which is bigger?
        bigger = if comparer zeros ones then '0' else '1'
        -- now filter and drop first character
        nextInput = map tail $ filter ((== bigger) . head) input
        c' = c ++ [bigger]
    
bitsToInt :: [Int] -> Int
bitsToInt = sum . zipWith (*) (map (2^) [0..]) . reverse

