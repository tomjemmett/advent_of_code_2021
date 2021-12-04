module Day03 (
  day03
) where

import Common
import Data.List (sort, group)

type Input = [String]
type StepFilter = Char -> [String] -> [String]
type Comparer = (Int -> Int -> Bool)

day03 :: AOCSolution
day03 input = [p1, p2] <*> pure i
  where
    i = lines input
    p1 = runPart (const id)                    -- we don't want to filter in part 1, so we can use the constant identity
    p2 = runPart (\b -> filter ((== b). head)) -- filter where the first item in the input matches the bigger item

runPart :: StepFilter -> Input -> String
runPart stepFilter input = show $ -- turn Int into a string
  product $                       -- multiply the values
  map bitStringToInt $            -- convert the bit strings to integers
  -- create two versions of our day 3 function for > and <= as the comparer argument
  -- we can use and Applicative to run each function on the LHS with the input (which needs to be wrapped in pure)
  map (d3 stepFilter "") [(>), (<=)] <*> pure input

d3 :: StepFilter -> -- a function which will filter the input list
  String         -> -- a string containing the current match (first iteration should be "")
  Comparer       -> -- a comparison function, should be either (>) or (<=)
  Input          -> -- the input string, where each item is a line from the file
  String            -- return a string of the result
d3 _ c _ ("":xs) = c  -- exhausted length of input strings, return the current match
d3 _ c _ [i] = c ++ i -- single element, return the current match with the single item
d3 stepFilter c comparer input = d3 stepFilter c' comparer nextInput -- recursively call
  where
    -- consider just the first character
    firstChars = map head input
    -- now find the number of 0's and 1's
    zeros = length $ filter (== '0') firstChars
    ones  = length firstChars - zeros
    -- which is bigger?
    bigger = if comparer zeros ones then '0' else '1'
    f = stepFilter bigger input
    -- now drop first character
    nextInput = map tail f
    c' = c ++ [bigger]
