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
processLine grid line = grid'
  where
    grid' = foldl processPoint grid points          -- process all of the points on this line
    points = lineToPoints line                      -- extract the points on this line
    processPoint = flip $ flip (M.insertWith (+)) 1 -- update the grid by incrementing the value at that position

-- convert a Line to the set of Points on that line
lineToPoints :: Line -> Points
lineToPoints (ab@(a, b), cd@(c, d))
  | ab == cd  = [ab] -- ab and cd are the same point, so just return it
  -- the following cases will handle where the cases where:
  -- * first direction isn't changed
  -- * second direction isn't changed
  -- * both directions change (diagonal)
  | a == c    = ab : lineToPoints ((a, f b d), cd)
  | b == d    = ab : lineToPoints ((f a c, b), cd)
  | otherwise = ab : lineToPoints ((f a c, f b d), cd)
  -- this function will either increment or decrement the first value to make it approach the second value
  where f x y = (if x < y then succ else pred) x

parseInput :: Applicative f => String -> f Lines
parseInput = pure . map (fromRight ((0,0), (0,0)) . P.parse p "") . lines
  where
    f [a, b, c, d] = ((a, b), (c, d))
    p :: P.CharParser () Line
    p = do
      a <- P.many1 P.digit -- read 1 or more digits
      P.char ','           -- comma is ignored
      b <- P.many1 P.digit -- read 1 or more digits
      P.string " -> "      -- arrow is ignored
      c <- P.many1 P.digit -- read 1 or more digits
      P.char ','           -- comma is ignored
      d <- P.many1 P.digit -- read 1 or more digits
      return $ f $ map read [a, b, c, d]