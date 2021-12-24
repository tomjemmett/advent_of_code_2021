module Day24 (
  day24
) where

import Common

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (chunksOf)
import Control.Monad.State (MonadState(get), State, modify, evalState)
import Data.Char (intToDigit)

type Step = (Int, Int, Int)

day24 :: AOCSolution 
day24 input = show . f <$> ([reverse, id] <*> pure [1..9])
  where
    i = parseInput input
    f x = foldl (\x y -> 10 * x + y) 0 $ head $ evalState (solve x 0 i) S.empty

parseInput :: String -> [Step]
parseInput = map f . chunksOf 18 . map words . lines
  where
    f :: [[String]] -> Step
    f i = tuplify3 $ map (\x -> read $ i !! x !! 2) [5, 4, 15]

solve :: [Int] -> Int -> [Step] -> State (S.Set (Int, Int)) [[Int]]
solve ws z (s : ss) = do
  dp <- get
  if S.member (z, length ss) dp
    then pure []
    else do
      result <- concat <$> traverse t ws
      modify (S.insert (z, length ss))
      pure result
  where
    t :: Int -> State (S.Set (Int, Int)) [[Int]]
    t w = if null ss
        then (if z' == 0 then pure [[w]] else pure [])
        else map (w :) <$> solve ws z' ss
      where
        z' = step z s w
    step :: Int -> Step -> Int -> Int
    step z (addX, divZ, addY) w = if x /= w
        then (z' * 26) + (w + addY)
        else z'
      where
        x  = (z `rem` 26) + addX
        z' = z `quot` divZ
