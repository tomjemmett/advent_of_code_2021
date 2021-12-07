module Common where

import Data.Foldable (toList)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

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

parse :: Parser a -> String -> Either P.ParseError a
parse = flip P.parse ""

-- assumes that the parser always succeeds, it applies a function f to the Right value from the parser
parse' :: Parser a -> (a -> b) -> String -> b
parse' p f s = case parse p s of Right x -> f x

number :: Parser Int
number = read <$> P.many1 P.digit

numbers :: String -> Parser [Int]
numbers s = number `P.sepBy` (P.many1 . P.oneOf) s