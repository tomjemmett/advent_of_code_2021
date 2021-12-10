module Day10 (
  day10
) where

import Common
import Data.List (sort)

data NavigationSystem = Complete | Incomplete | Corrupted deriving (Show, Eq)

day10 :: AOCSolution
day10 input = show <$> [p1, p2]
  where
    i = map (parseLine "") $ lines input
    p1 = sum $           map snd $ filter ((== Corrupted ) . fst) i
    p2 = head $ median $ map snd $ filter ((== Incomplete) . fst) i

parseLine :: String -> String -> (NavigationSystem, Int)
parseLine [] [] = (Complete, 0)
parseLine st [] = (Incomplete, incompleteScore st)
parseLine st (x:xs)
  | x `elem` "([<{"      = parseLine (x:st) xs
  | x == ')' && s == '(' = parseLine ss xs
  | x == ']' && s == '[' = parseLine ss xs
  | x == '>' && s == '<' = parseLine ss xs
  | x == '}' && s == '{' = parseLine ss xs
  | otherwise            = (Corrupted, corruptedScore x)
  where
    (s:ss) = st

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