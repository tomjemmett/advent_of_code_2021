module Common (
  AOCSolution,
  countTrue,
  linesRead,
  linesWords,
  numbersStringToInt,
  wordSeparatedInts,
  commaSeparatedInts,
  bitStringToInt,
  bitsToInt,
  map2
) where

import Data.Foldable (toList)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)

type AOCSolution = String -> [String]

countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

linesRead :: Read a => String -> [a]
linesRead = map read . lines

linesWords :: String -> [[String]]
linesWords = map words . lines

numbersStringToInt :: (String -> [String]) -> String -> [Int]
numbersStringToInt split = map read . split

wordSeparatedInts :: String -> [Int]
wordSeparatedInts = numbersStringToInt words

commaSeparatedInts :: String -> [Int]
commaSeparatedInts = numbersStringToInt (splitOn ",")

bitStringToInt :: String -> Int
bitStringToInt = bitsToInt . map digitToInt

bitsToInt :: [Int] -> Int
bitsToInt = sum . zipWith (*) (map (2^) [0..]) . reverse

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)
