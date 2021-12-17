-- module Day17 (
--   day17
-- ) where
module Day17 where

import Common
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type VelocityVector = (Int, Int)
type ValidRange = ((Int, Int), (Int, Int))

day17 :: AOCSolution
day17 input = show <$> [p1, p2]
  where
    i = search $ parseInput input
    p1 = fromMaybe 0 $ maximum i
    p2 = length $ catMaybes i

parseInput :: String -> ValidRange
parseInput = tuplify2 . map tuplify2 . map2 read . map (splitOn ".." . drop 2) . splitOn ", " . drop 13

search :: ValidRange -> [Maybe Int]
search tr = concat [[findMaxY tr (vx, vy) | vx <- [0..260]] | vy <- [(-120)..120]]

findMaxY :: ValidRange -> VelocityVector -> Maybe Int
findMaxY ((minX, maxX), (minY, maxY)) = step 0 False (0, 0)
  where
    step :: Int -> Bool -> Point2d -> VelocityVector -> Maybe Int
    step r b (px, py) v@(vx, vy)
      | px > maxX = if b then Just r else Nothing
      | py < minY = if b then Just r else Nothing
      | otherwise = step r' b' p' v'
      where
        p' = (px + vx, py + vy)
        v' = updateVelocityVector v
        r' = maximum [r, py]
        b' = b || (px >= minX && px <= maxX && py >= minY && py <= maxY)

updateVelocityVector :: VelocityVector -> VelocityVector
updateVelocityVector (x, y) = (f x, pred y)
  where
    f x = case compare x 0 of
      GT -> maximum [pred x, 0]
      LT -> minimum [succ x, 0]
      EQ -> x