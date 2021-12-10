module Day10 (
  day10
) where

import Common
import Data.List (sort)
import Control.Monad (foldM)

data NavigationSystem = Incomplete | Corrupted deriving (Show, Eq)

day10 :: AOCSolution
day10 input = map show $ f <$> [(sum, Corrupted), (head . median, Incomplete)] <*> pure i
  where
    i = map parseLine $ lines input
    f (s, x) = s . map snd . filter ((== x) . fst)

parseLine :: String -> (NavigationSystem, Int)
parseLine l = case foldM p "" l of
  Left lastChar  -> (Corrupted, corruptedScore lastChar)
  Right stack    -> (Incomplete, incompleteScore stack)
  where
    p st x
      | x `elem` "([<{"   = Right (x:st)
      | validPair (s:[x]) = Right ss
      | otherwise         = Left x
      where
        (s:ss) = st

validPair :: String -> Bool
validPair = flip elem ["()", "[]", "<>", "{}"]

corruptedScore :: Char -> Int
corruptedScore = \case
  ')' ->     3
  ']' ->    57
  '}' ->  1197
  '>' -> 25137

incompleteScore :: String -> Int
incompleteScore = foldl (\x y -> x * 5 + f y) 0
  where
    f = \case
      '(' -> 1
      '[' -> 2
      '{' -> 3
      '<' -> 4