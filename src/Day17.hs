module Day17 (
  day17
) where

import Common
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type VelocityVector = (Int, Int)
type ValidRange = ((Int, Int), (Int, Int))

day17 :: AOCSolution
day17 input = show <$> ([part1, part2] <*> parseInput input)

parseInput :: Applicative f => String -> f ValidRange
parseInput = pure . tuplify2 . map (tuplify2 . numbersStringToInt (splitOn "..") . drop 2) . splitOn ", " . drop 13

triangleN :: Int -> Int
triangleN x = x * (x + 1) `div` 2

part1 :: ValidRange -> Int 
part1 (_, (y, _)) = triangleN $ pred $ abs y

part2 :: ValidRange -> Int
part2 (xr, yr) = S.size $ S.union a b
  where
    ys = concat [[(i, i + j), (-i - 1, j - i - 1)] |
      j <- [0..abs $ fst yr],
      i <- [0..pred j],
      let (ti, tj) = (triangleN i, triangleN j),
      fst yr <= ti - tj,
      ti - tj <= snd yr]

    xs = [(i, j) |
      i <- [0..snd xr],
      j <- [0..pred i],
      let (ti, tj) = (triangleN i, triangleN j),
      fst xr <= ti - tj,
      ti - tj <= snd xr]

    xk = M.fromListWith (++) $ map (\(i, j) -> (i - j - 1, [i])) xs
    zeros = map fst $ filter ((== 0) . snd) xs

    a = S.fromList $ concat $ mapMaybe f ys
      where
        f (y, k) = map (,y) <$> (xk M.!? k)

    b = S.fromList $ concatMap (\(y, _) -> map (,y) zeros) $ filter ((>= minimum zeros) . snd) ys