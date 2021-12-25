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
step = stepSouth . stepEast

stepEast :: (Seafloor, Bounds) -> (Seafloor, Bounds)
stepEast (arr, (by, bx)) = (arr', (by, bx))
  where
    arr' = M.fromList do
      ((y, x), v) <- M.toList arr

      let [px, nx] = map (`mod` bx) $ [pred, succ] <*> pure x
          [pv, nv] = map ((arr M.!) . (y, )) [px, nx]
          v' = if | v == '>' && nv == '.' -> '.'
                  | v == '.' && pv == '>' -> '>'
                  | otherwise             -> v

      return ((y, x), v')

stepSouth :: (Seafloor, Bounds) -> (Seafloor, Bounds)
stepSouth (arr, (by, bx)) = (arr', (by, bx))
  where
    arr' = M.fromList do
      ((y, x), v) <- M.toList arr

      let [py, ny] = map (`mod` by) $ [pred, succ] <*> pure y
          [pv, nv] = map ((arr M.!) . (, x)) [py, ny]
          v' = if | v == 'v' && nv == '.' -> '.'
                  | v == '.' && pv == 'v' -> 'v'
                  | otherwise             -> v

      return ((y, x), v')

debug :: (Seafloor, Bounds) -> IO ()
debug (arr, bounds) = putStrLn $ unlines [[arr M.! (y, x) | x <- [0..pred $ snd bounds]] | y <- [0..pred $ fst bounds]]

testInput = "v...>>.vv>\n\
            \.vv>>.vv..\n\
            \>>.>v>...v\n\
            \>>v>>.>.v.\n\
            \v>v.vv.v..\n\
            \>.>>..v...\n\
            \.vv..>.>v.\n\
            \v.v..>>v.v\n\
            \....v..v.>"

i = parseInput testInput
b = getBounds i

{-
....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v
-}