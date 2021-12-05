module Day05 (
  day05
) where

import Common
import Data.Either (fromRight)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Text.ParserCombinators.Parsec as P

type Point  = (Int, Int)
type Points = [Point]
type Line   = (Point, Point)
type Lines  = [Line]
type Grid   = M.Map Point Int

day05 :: AOCSolution
day05 input = fmap (show . d5) $ [f, id] <*> parseInput input
  where
    -- for part 1, remove diagnoal lines
    f = filter (\((a, b), (c, d)) -> a == c || b == d)

d5 :: Lines -> Int
d5 = length .                -- get the length of remaining items in the list, our result
  filter((>= 2)) .           -- remove points from our "grid" which were matched less than twice
  M.elems .                  -- get the elements from the map
  foldl processLine M.empty  -- use an empty map to store our grid, then fold over the input (lines) to update the grid

processLine :: Grid -> Line -> Grid
processLine grid = foldl processPoint grid . lineToPoints
  where
    -- update the grid by incrementing the value at the given position. if the positions isn't already in the map, then
    -- a value of 1 is inserted
    processPoint = flip $ flip (M.insertWith (+)) 1

-- convert a Line to the set of Points on that line
lineToPoints :: Line -> Points
lineToPoints (ab@(a, b), cd@(c, d)) = ab : if ab == cd
  then []                                -- when ab == cd, nothing more to do
  else lineToPoints ((f a c, f b d), cd) -- use the f function below to alter a and b to apprach c and d
  where
    f x y
      | x < y = succ x -- move a/b closer to c/d by incrementing
      | x > y = pred x -- move a/b closer to c/d by decrementing
      | otherwise = x  -- a/b has reached c/d, so stay there

-- use parsec to parse our input into the Lines data type
parseInput :: Applicative f => String -> f Lines
parseInput = pure . map (fromRight ((0,0), (0,0)) . P.parse p "") . lines
  where
    p :: P.CharParser () Line
    p = do
      a <- P.many1 P.digit -- read 1 or more digits
      P.char ','           -- comma is ignored
      b <- P.many1 P.digit -- read 1 or more digits
      P.string " -> "      -- arrow is ignored
      c <- P.many1 P.digit -- read 1 or more digits
      P.char ','           -- comma is ignored
      d <- P.many1 P.digit -- read 1 or more digits
      return $ (newPoint a b, newPoint c d)

newPoint :: String -> String -> Point
newPoint x y = (read x, read y)