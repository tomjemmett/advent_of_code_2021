-- module Day25 (
--   day25
-- ) where
module Day25 where

import Common
import qualified Data.Map as M
import Data.Bifunctor (bimap)

type Seafloor = M.Map Point2d Char
type Bounds = Point2d

day25 :: AOCSolution
day25 = pure . show . part1 . parseInput

parseInput :: String -> Seafloor
parseInput = M.fromList . concat . (zipWith3 . zipWith3) f (repeat [0..]) (map repeat [0..]) . lines
  where
    f x y = ((y, x),)

getBounds :: Seafloor -> Bounds
getBounds = bimap succ succ . last . M.keys

part1 :: Seafloor -> Int
part1 arr = go (arr, bounds) 1
  where
    bounds = getBounds arr
    go :: (Seafloor, Bounds) -> Int -> Int
    go (a, b) n = if a == a'
        then n
        else go (a', b) $ succ n
      where
        (a', _) = step (a, b)

step :: (Seafloor, Bounds) -> (Seafloor, Bounds)
step = step' 'v' . step' '>'

step' :: Char -> (Seafloor, Bounds) -> (Seafloor, Bounds)
step' chr (arr, (by, bx)) = (arr', (bx, by))
  where
    arr' = M.fromList do
      ((y, x), v) <- M.toList arr

      let [px, nx] = map (`mod` bx) $ [pred, succ] <*> pure x
          [pv, nv] = map ((arr M.!) . (y, )) [px, nx]
          v' = if | v == chr && nv == '.' -> '.'
                  | v == '.' && pv == chr -> chr
                  | otherwise             -> v
      
      return ((x, y), v')

debug :: (Seafloor, Bounds) -> IO ()
debug (arr, bounds) = putStrLn $ unlines [[arr M.! (y, x) | x <- [0..pred $ snd bounds]] | y <- [0..pred $ fst bounds]]