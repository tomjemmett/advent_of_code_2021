module Day04 (
  day04
) where

import Common
import Data.Function (on)
import Data.List (transpose, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)

type Numbers = [Int]
type Line    = [Int]
type Board   = [Line]
type Boards  = [Board]
type Results = [Int]

testInput = unlines ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
                     "",
                     "22 13 17 11  0",
                     " 8  2 23  4 24",
                     "21  9 14 16  7",
                     " 6 10  3 18  5",
                     " 1 12 20 15 19",
                     "",
                     " 3 15  0  2 22",
                     " 9 18 13 17  5",
                     "19  8  7 25 23",
                     "20 11 10 24  4",
                     "14 21 16 12  6",
                     "",
                     "14 21 17 24  4",
                     "10 16 15  9 19",
                     "18  8 23 26 20",
                     "22 11 13  6  5",
                     " 2  0 12  3  7"]


-- solves day 4 by parsing the input, running the d4 function (which solves everyboard), then takes just the first and
-- the last item's (for part 1 and part 2 respectively), before turning the int's back into string's
day04 :: AOCSolution
day04 input = show <$> ([head, last] <*> pure (d4 $ parseInput input))

-- our input contains one line of comma separated values
-- followed by two new lines
-- followed by a 5x5 grid of space separated numbers
-- and, then more 5x5 grids separated each time by two new lines
-- this will turn that into a tuple containing a list of the numbers,
-- followed by a list of the boards
-- each board will be represented as each line as a list of strings,
-- concatenated with each column as a list of strings
parseInput :: String -> (Numbers, Boards)
parseInput input = (n, zipWith (++) bs cs)
  where
    i  = splitOn "\n\n" input
    n  = numbersStringToInt (splitOn ",") $ head i
    bs = map (filter (/= []) . map (numbersStringToInt words) . splitOn "\n") $ tail i
    cs = map transpose bs

-- out function to play bingo
d4 :: (Numbers, Boards) -> Results
-- base case: we have played all of the cards
d4 (_, []) = []
-- case where we still have cards to play
d4 ((n:nums), boards) = case checkBingo of
  -- one of the cards has called bingo, return the results for that and continue solving
  Just b -> result b : d4 (nums, notBingo)
  -- no cards have called bingo, so keep playing
  Nothing -> d4 (nums, nextBoards)
  where
    -- find the next iteration of the boards afting calling "n"
    nextBoards = callNumber n boards
    -- create a function to filter the boards, either for when we have bingo, or when we don't have bingo
    filterBoards comp = filter (comp 0 . minimum . map length) nextBoards
    [isBingo, notBingo] = filterBoards <$> [(==), (/=)]
    -- turn isBingo into a Maybe monad
    checkBingo = listToMaybe isBingo
    -- function to calculate the results: we flatten the list of lists into a single list,
    -- sum the results
    -- then divide by 2 (as we double the list to have both the rows and columns, we count everything twice)
    -- finally we multiple by the current "n"
    result = (* n) . flip div 2 . sum . concat

-- when a number is called, filter out that number from a board
callNumber :: Int -> Boards -> Boards
callNumber n boards = map f boards
  where
    f ls = map (filter (/= n)) ls
