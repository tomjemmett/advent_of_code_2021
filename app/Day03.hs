module Day03 (
  day03
) where

import Common
import Data.List (sort, group)

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
part1 input = show $ gamma input * epsillon input
  where
    gamma = bitStringToInt . p1 (>) ""
    epsillon = bitStringToInt . p1 (<=) ""
    p1 = d3 (const id) -- we don't want to filter in part 1, so we can use the constant identity

part2 :: [String] -> String
part2 input = show $ oxygen input * co2Scruber input
  where
    oxygen = bitStringToInt . p2 (>) ""
    co2Scruber = bitStringToInt . p2 (<=) ""
    p2 = d3 (\b -> filter ((== b) . head)) -- filter where the first item in the input matches the bigger item

d3 :: (Char -> [String] -> [String]) -> -- a function which will filter the input list
  (Int -> Int -> Bool) ->               -- a comparison function, should be either (>) or (<=)
  String ->                             -- a string containing the current match (first iteration should be "")
  [String] ->                           -- the input string, where each item is a line from the file
  String                                -- return a string of the result
d3 _ _ c ("":xs) = c  -- exhausted length of input strings, return the current match
d3 _ _ c [i] = c ++ i -- single element, return the current match with the single item
d3 stepFilter comparer c input = d3 stepFilter comparer c' nextInput -- recursively call
  where
    -- consider just the first character
    firstChars = map head input
    -- now find the number of 0's and 1's
    zeros = length $ filter (== '0') firstChars
    ones  = length $ filter (== '1') firstChars
    -- which is bigger?
    bigger = if comparer zeros ones then '0' else '1'
    f = stepFilter bigger input
    -- now drop first character
    nextInput = map tail f
    c' = c ++ [bigger]
