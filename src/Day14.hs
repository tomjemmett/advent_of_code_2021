module Day14 (
  day14
) where

import Common
import Control.Arrow (first)
import Data.List.Split (splitOn)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Data.Map as M
import Data.Map ((!?))

type Pair = (Char, Char)
type Counter = M.Map Pair Int
type Rules = M.Map Pair [Pair]

day14 :: AOCSolution
day14 input = show . answer . (results !!) <$> [10, 40]
  where
    (target, rules) = parseInput input
    results = iterate (d14 rules) target

answer :: Counter -> Int
answer = (\x -> maximum x - minimum x) .
  map snd .
  M.toList .
  M.fromListWith (+) .
  map (first snd) .
  M.toList

parseInput :: String -> (Counter, Rules)
parseInput input = (counter, rules')
  where
    [target, rules] = splitOn "\n\n" input
    counter = M.fromListWith (+) $ zipWith (curry (,1)) (' ' : target) target
    rules' = M.fromList $ map (toRule . splitOn " -> ") $ lines rules
    toRule [[a, b], [c]] = ((a, b), [(a, c), (c, b)])

d14 :: Rules -> Counter -> Counter
d14 m = M.fromListWith (+) .
  concatMap (\(p, c) -> maybe [(p, c)] (map (,c)) (m !? p)) .
  M.toList