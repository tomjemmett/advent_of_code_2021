module Day22 (
  day22
) where

import Common
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Debug.Trace

type ValueRange  = (Int, Int)
type ValueRanges = (ValueRange, ValueRange, ValueRange)
type Input       = (OnOff, ValueRanges)
data OnOff       = On | Off deriving (Show, Eq)

day22 :: AOCSolution
day22 input = show . solve <$> (filter <$> [p1 .snd , p2] <*> pure (parseInput input))
  where
    p1 ((xs, xe), (ys, ye), (zs, ze)) = all ((<=50) . abs) [xs, xe, ys, ye, zs, ze]
    p2 = const True

class Range a where
  intersect :: a -> a -> Maybe a
  area :: a -> Int

instance Range ValueRange where
  intersect (s0, e0) (s1, e1)
    | s1 > e0 || s0 > e1 = Nothing
    | otherwise          = Just (max s0 s1, min e0 e1)
  area (s, e) = e - s + 1

instance Range ValueRanges where
  intersect (x0, y0, z0) (x1, y1, z1) = do
    x <- x0 `intersect` x1
    y <- y0 `intersect` y1
    z <- z0 `intersect` z1
    return (x, y, z)
  area (x, y, z) = product $ map area [x, y, z]

parseInput :: String -> [Input]
parseInput = map (parse' p id) . lines
  where
    p = do
      onoff <- P.try (On <$ P.string "on") P.<|> Off <$ P.string "off"
      P.char ' '
      vals <- tuplify3 <$> range `P.sepBy` P.char ','
      return (onoff, vals)
    range = do
      P.oneOf "xyz"
      P.char '='
      a <- num
      P.string ".."
      b <- num
      return (a, b)
    num = do
      n <- P.option 1 ((-1) <$ P.char '-')
      v <- number
      return (n * v)

solve :: [Input] -> Int
solve = sum . map (\(r, s) -> area r * s) . foldl go []
    where
      go :: Range r => [(r, Int)] -> (OnOff, r) -> [(r, Int)]
      go m (On,  r) = turnon  m r
      go m (Off, r) = turnoff m r

      turnoff :: Range r => [(r, Int)] -> r -> [(r, Int)]
      turnoff m r = mapMaybe f m ++ m
        where f (a, b) = (, negate b) <$> (a `intersect` r)

      turnon :: Range r => [(r, Int)] -> r -> [(r, Int)]
      turnon m r = (r, 1) : turnoff m r